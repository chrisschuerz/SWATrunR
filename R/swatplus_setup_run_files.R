#' Initialize the SWAT+ settings input files
#'
#' The routine reads the settings input files file.cio, print.prt, and time.sim
#' from the project folder. Based on the defined input arguments of the SWAT+
#' simulation in R, parameters in the input files are changed. An initial
#' calibration.cal file is generated if parameter values should be changed.
#' The updated input files are saved in a list for further writing of the input
#' setting files in the thread folders.
#'
#' @param project_path Path to the SWAT project folder on the hard drive
#'   (i.e. txtinout folder).
#'
#' @param start_date,end_date Start and end dates of the SWAT simulation.
#'
#'   Start and end date always have to be provided together. Default values are
#'   `NULL`. In this case the projects' date definition is used.
#'
#'   The date can be provided as character string in any ymd format
#'   (e.g. 'yyyy-mm-dd'), as a numeric value (yyyymmdd) or in a `Date`
#'   format.
#'
#' @param start_date_print Start date for printing SWAT simulations.
#'
#'   `start_date_print` is always overruled by the definition of `years_skip`.
#'   The default value of `start_date_print` is `NULL`. In this case either the
#'   `years_skip` is used, or if both are `NULL` the projects' definition of
#'   output printing is used.
#'
#'   The date can be provided as character string in any ymd format
#'   (e.g. 'yyyy-mm-dd'), as a numeric value (yyyymmdd) or in a `Date`
#'   format.
#'
#' @param years_skip Number of simulated years to skip in the printed outputs.
#'
#'   An integer value to define the number of years. Default value is `NULL`.
#'   In this case either `start_date_print` is used or the projects' definition
#'   of output printing is used.
#'
#' @param unit_conds List of possible values for condition variables ('hsg',
#'   'texture', 'plant', 'landuse') and object unit ids ('hru', 'sol', 'cha',
#'   'res', and 'aqu') which will be used in the calibration.cal for
#'   conditioning of parameter changes.
#'
#' @importFrom lubridate as_date ceiling_date int_end int_start interval
#'   leap_year yday year years ymd
#' @importFrom dplyr case_when mutate select %>%
#' @importFrom purrr map_chr set_names
#' @importFrom readr read_lines read_table
#' @importFrom stringr str_sub
#'
#' @keywords internal
#'
#' @returns A list with the prepared SWAT+ input settings:
#'
#'  - `$file.cio` Updated file.cio as string vector
#'  - `$print.prt` Updated print.prt as string vector
#'  - `$time.sim` Updated time.sim as string vector
#'  - `$start_date` Date value
#'  - `$end_date` Date value
#'  - `$years_skip` Numeric value
#'  - `$start_date_print` Date value
#'  - `$calibration.cal` Tibble with initial structure of calibration.cal which
#'    will be completed in a simulation run with the respective parameter values.
#'
setup_swatplus <- function(project_path, parameter, output,
                           start_date, end_date, start_date_print,
                           years_skip, unit_conds) {
  ## Read unmodified file.cio, print.prt, and time.sim
  options(readr.num_columns = 0)
  model_setup <- list()
  model_setup$file.cio <- read_lines(project_path%//%"file.cio", lazy = FALSE)
  model_setup$print.prt <- read_lines(project_path%//%"print.prt", lazy = FALSE)
  model_setup$time.sim <- read_lines(project_path%//%"time.sim", lazy = FALSE)

  ## Add calibration.cal to the file.cio
  ## This ensures that the calibration.cal that is added to indicate
  ## parameter changes are read by SWAT+
  model_setup$file.cio[22] <-
    'chg               cal_parms.cal     calibration.cal   null              null              null              null              null              null              null'

  print_table <- model_setup$print.prt[- c(1:10)] %>%
    str_trim(.) %>%
    str_split(., pattern = '[:space:]+') %>%
    map_df(., ~tibble(objects = .x[1], day = .x[2],
                      mon = .x[3], yr = .x[3], aa = .x[4]))

  ## Define simulation period
  if(xor(is.null(start_date), is.null(end_date))) {
    stop("'start_date' and 'end_date' must be provided together!")
  } else if (!is.null(start_date)) {
    ## Determine required date indices for writing to time.sim
    start_date <- ymd(start_date)
    end_date   <- ymd(end_date)
    time_interval <- interval(start_date, end_date)
    start_year    <- year(int_start(time_interval))
    start_jdn     <- yday(int_start(time_interval))
    end_year      <- year(int_end(time_interval))
    end_jdn       <- yday(int_end(time_interval))

    model_setup$time.sim[3] <- c(start_jdn, start_year, end_jdn, end_year, 0) %>%
      sprintf("%10d",.) %>%
      paste(., collapse = "")
  } else {
    t_sim <- model_setup$time.sim[3] %>%
      strsplit(., "\\s+") %>%
      unlist(.) %>%
      .[nchar(.) > 0] %>%
      as.numeric(.)

    start_year <- t_sim[2]
    end_year   <- t_sim[4]

    start_day <- ifelse(t_sim[1] > 0, t_sim[1], 1)
    is_leap <- leap_year(end_year)
    end_day <- ifelse(t_sim[3] > 0, t_sim[3], ifelse(is_leap, 366, 365))

    start_date <- as_date(x = start_year%//%start_day, format = "%Y/%j")
    end_date   <- as_date(x = end_year%//%end_day, format = "%Y/%j")
  }
  model_setup$start_date <- start_date
  model_setup$end_date   <- end_date

  ## Check the numbers of years to skip with the simulation interval Overwrite
  ## number of years to skip if value was provided otherwise get value from
  ## print file
  ## Other print options e.g. printing smaller time frame or other intervals
  ## than 1 are omitted, as any information loss should be avoided. Filtering
  ## of time windows can be done later in a modeling workflow
  if(is.null(years_skip) & is.null(start_date_print)) {
    print_time <- model_setup$print.prt[3] %>%
      strsplit(., "\\s+") %>%
      unlist(.) %>%
      .[nchar(.) > 0] %>%
      as.numeric(.)

    years_skip <- print_time[1]
    if (years_skip > 0) {
      print_jdn  <- 0
      print_year <- 0
      start_date_print <- NA_real_
    } else {
      print_jdn  <- max(print_time[2], 1)
      print_year <- max(print_time[3], year(start_date))
      start_date_print <- as_date(x = print_year%//%print_jdn, format = "%Y/%j")
      years_skip <- NA_integer_
    }

  } else if(!is.null(start_date_print)){
    start_date_print <- ymd(start_date_print)

    print_jdn  <- yday(start_date_print)
    print_year <- year(start_date_print)
    years_skip <- NA_integer_

  } else {
    print_jdn  <- 0
    print_year <- 0
    start_date_print <- NA_real_
  }

  if (!is.na(years_skip)) {
    if (years_skip < 0) {
      stop("'years_skip' must be a positive value.")
    }
    if (years_skip + year(start_date) > year(end_date)) {
      stop("'start_date' + 'years_skip' is later than 'end_date'.")
    }
  }

  if(!is.na(start_date_print)) {
    if(start_date_print < start_date) {
      stop("'start_date_print' is an earlier date than 'start_date'.")
    }
    if(start_date_print > end_date) {
      stop("'start_date_print' is a later date than 'end_date'.")
    }
  }

  model_setup$print.prt[3] <-
    c(max(years_skip, 0, na.rm = TRUE), print_jdn, print_year, 0, 0, 1) %>%
    sprintf("%-11d", .) %>%
    paste(., collapse = '')

  model_setup$years_skip <- years_skip
  model_setup$start_date_print <- start_date_print

  ## Output interval settings
  ## Set output_interval to 'daily' as default if not provided by user.
  # if(is.null(output_interval)) output_interval <- "d"
  #
  # if(output_interval == 'm') {
  #   t_int <- seq(start_date_print, end_date, by = 'm')
  #   if(length(t_int) == 1 & end_date != ceiling_date(end_date, unit = 'm')-1) {
  #     stop('Monthly simulation outputs require a simulation period of ',
  #          'at least one full month!')
  #   }
  # }
  print_table[,2:5] <- "n"

  # Remove crop, mgt, and FDC outputs from objects as they are not defined there
  objects_in_tbl <- filter(output, ! file %in% c('basin_crop_yld', 'fdcout', 'mgtout'))
  # Change from channel_sdmorph to channel_sd as the first is written when the
  # second is defined
  objects_in_tbl[objects_in_tbl == 'channel_sdmorph'] <- 'channel_sd'

  if(nrow(objects_in_tbl) > 0) {
    for(i in 1:nrow(objects_in_tbl)) {
      print_table[print_table$objects == objects_in_tbl$file[i],
                  objects_in_tbl$time_interval[i]] <- 'y'
    }
  }

  # Set all outputs to no, except the output files defined in output and only
  # for the defined time interval
  # print_table[print_table$objects %in% object_names, output_interval] <- "y"

  print_table <- print_table %>%
    mutate(objects = sprintf("%-16s", objects),
           day = sprintf("%14s", day),
           mon = sprintf("%14s", mon),
           yr  = sprintf("%14s", yr),
           aa  = sprintf("%14s", aa)) %>%
    apply(., 1, paste, collapse = "")

  model_setup$print.prt <- c(model_setup$print.prt[1:10], print_table)

  # So far avoid any other output files types to be written
  model_setup$print.prt[7] <- "n             n             n             "

  # Print also FDC and mgt output files if defined in outputs
  prt_9 <- rep('n', 4)
  if ('mgtout' %in% output$file) prt_9[2] <- 'y'
  if ('fdcout' %in% output$file) prt_9[4] <- 'y'
  prt_9 <- paste(sprintf('%-14s',prt_9), collapse = '')

  model_setup$print.prt[9] <- prt_9

  if(!is.null(parameter)) {
    parameter$definition <- filter(parameter$definition, file_name != 'pdb')
    model_setup$calibration.cal <- map(1:nrow(parameter$definition),
                                       ~ parameter$definition[.x,]) %>%
      map(., ~ setup_calibration_cal(.x, unit_conds)) %>%
      bind_rows(.)
  }

  return(model_setup)
}

#' Initialize a line in the calibration.cal table for the parameter i and add
#' parameter conditions
#'
#' The function takes the definition of parameter i and translates it into a
#' tibble row while leaving the change value for the parameter `NA`. This will
#' be filled in when running the simulation.
#' If any conditions are defined for the parameter additional lines with the
#' constraints are added in the tibble.
#'
#' @param par_def_i Definition of parameter i (i.e. the ith line of parameter$definition)
#'
#' @param unit_conds List of possible values for condition variables ('hsg',
#'   'texture', 'plant', 'landuse') and object unit ids ('hru', 'sol', 'cha',
#'   'res', and 'aqu') which will be used in the calibration.cal for
#'   conditioning of parameter changes.
#'
#' @importFrom dplyr bind_rows select %>%
#' @importFrom purrr map map2_df
#' @importFrom tibble tibble
#' @importFrom tidyselect any_of
#'
#' @keywords internal
#'
#' @returns A tibble with initial lines for calibration.cal for the parameter i.
#'
setup_calibration_cal <- function(par_def_i, unit_conds) {
  par_cal <- init_calibration_cal(par_def_i)

  if('lyr' %in% names(par_def_i)) {
    if(!is.na(par_def_i$lyr)) {
      par_cal <- add_value_range(par_cal, par_def_i$lyr, 'LYR')
    }
  }
  if('year' %in% names(par_def_i)) {
    if(!is.na(par_def_i$year)) {
      par_cal <- add_value_range(par_cal, par_def_i$year, 'YEAR')
    }
  }
  if('day' %in% names(par_def_i)) {
    if(!is.na(par_def_i$day)) {
      par_cal <- add_value_range(par_cal, par_def_i$day, 'DAY')
    }
  }
  if('unit' %in% names(par_def_i)) {
    if(!is.na(par_def_i$unit)) {
      par_cal <- add_obj(par_cal, par_def_i$unit,
                         unit_conds$units[[par_def_i$file_name]])
    }
  }

  if(any(c('hsg', 'texture', 'plant', 'landuse') %in% names(par_def_i))) {
      soil_luse <- select(par_def_i, any_of(c('hsg', 'texture', 'plant', 'landuse'))) %>%
        select(., !tidyselect:::where(is.na)) %>% # will be replaced when where is in the tidyselect namespace
        map(., ~ .x)
      cond_tbl <- map2_df(soil_luse, names(soil_luse), ~ add_soil_luse(.x, .y, unit_conds$conds))
  } else {
    cond_tbl <- tibble()
  }

  if(any('slope' %in% names(par_def_i))) {
    if(!is.na(par_def_i$slope)) {

      cond_tbl <- add_slope(par_def_i$slope, cond_tbl)
    }
  }

  if(any(c('hsg', 'texture', 'plant', 'landuse', 'slope') %in% names(par_def_i))) {
    par_cal$CONDS <- as.character(nrow(cond_tbl))
    par_cal <- bind_rows(par_cal, cond_tbl)
  }

  return(par_cal)
}

#' Initialize a line in the calibration.cal table for the parameter i
#'
#' @param par_def_i Definition of parameter i (i.e. the ith line of parameter$definition).
#'
#' @importFrom dplyr mutate select %>%
#' @importFrom purrr set_names
#' @keywords internal
#'
#' @returns A tibble with one line which initializes the parameter i.
#'
init_calibration_cal <- function(par_def_i) {
  par_def_i %>%
  select(., parameter, change) %>%
    set_names(c("NAME", "CHG_TYPE")) %>%
    mutate(VAL = NA_real_, CONDS = '0', LYR1 = 0, LYR2 = 0, YEAR1 = 0, YEAR2 = 0,
           DAY1 = 0, DAY2 = 0, OBJ_TOT = 0)
}

#' Add values for parameter conditions 'lyr', 'year', 'day', or 'unit' to the
#' initialized calibration.cal line of parameter i.
#'
#' @param par_cal_i The calibration.cal one line tibble for the parameter i.
#' @param val Vector that defines the value range.
#' @param var Name of the variable for which the value range should be added.
#'
#' @keywords internal
#'
#' @returns The calibration.cal one line tibble for the parameter i with the
#' updated value range for the variable 'var'.
#'
add_value_range <- function(par_cal_i, val, var) {
  val_range <- get_value_range(val)
  par_cal_i[var%&%'1'] <- val_range[1]
  par_cal_i[var%&%'2'] <- val_range[2]
  return(par_cal_i)
}

#' Extract the value range for a condition from the parameter definition table
#' `parameter$definition`.
#'
#' The conditions are defined as text strings with Boolean operators. These are
#' removed and the remaining values are evaluated and returned.
#'
#' @param cond Character string that defines a condition.
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_detect str_remove
#' @importFrom purrr set_names
#' @keywords internal
#'
#' @returns A numeric vector with the unit values
#'
get_value_range <- function(cond) {
  if(str_detect(cond, '%in%')) {
    cond <- cond %>%
      str_remove(., '%in%') %>%
      parse(text = .) %>%
      eval(.)
  } else if (str_detect(cond, '==')) {
    cond <- cond %>%
      str_remove(., '==') %>%
      as.numeric(.)
  } else {
    stop("For parameter conditioning with 'lyr', 'year', and 'day' only single",
         "values or upper lower bound implemented yet!")
  }
  return(c(min(condition), max(condition)))
}

#' Add the unit values for which objects the parameter change should be applied
#'
#' @param par_cal_i The calibration.cal one line tibble for the parameter i.
#' @param unit Character string that defines condition for the object units (ids of e.g. hru objects)
#' @param unit_all List of vectors that define the unit ids of all object types.
#'
#' @importFrom dplyr bind_cols bind_rows %>%
#' @importFrom purrr set_names
#'
#' @keywords internal
#'
#' @returns The calibration.cal one line tibble for the parameter i with the
#' updated 'OBJ_*' columns which define the object units for parameter change.
#'
add_obj <- function(par_cal_i, unit, unit_all) {
  eval_unit <- paste('unit_all', unit) %>%
  parse(text = .) %>%
  eval(.)

  unit <- unit_all[eval_unit] %>%
    unique(.) %>%
    sort(.) %>%
    group_units(.) %>%
    set_names(., 'OBJ'%_%1:length(.)) %>%
    bind_rows()

  par_cal_i$OBJ_TOT <- length(unit)
  par_cal_i <- bind_cols(par_cal_i, unit)

  return(par_cal_i)
}

#' Group sequences of units
#'
#' The unit condition in calibration.cal follows a specific syntax. This function
#' translates a vector of units into groups with that syntax for writing the OBJ
#' columns in calibration.cal
#'
#' @param val Numeric vector with the unit values
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map map2 reduce
#'
#' @keywords internal
#'
#' @returns A numeric vector with the required syntax (e.g. 1 to 5 and 8 is
#'   c(1, -5, 8))
#'
group_units <- function(val) {
  split_end <- which(diff(val) > 1)
  split_start <- c(1, split_end + 1)
  split_end <- c(split_end, length(val))
  map2(split_start, split_end, ~ val[.x:.y]) %>%
  map(., ~ translate_sequence(.x)) %>%
  reduce(., c)
}

#' Translate the unit sequences to calibration.cal syntax
#'
#' @param val_seq Numeric vector with the unit values.
#'
#' @keywords internal
#'
#' @returns A numeric vector of length 2 with c(min(val_seq), -max(val_seq))
#'
translate_sequence <- function(val_seq) {
  if (length(val_seq) > 1) {
    val_seq <- c(min(val_seq), -max(val_seq))
  }
  return(val_seq)
}

#' Add condition lines based on soil and land use variables
#'
#' The function translates the condition `cond` defined for the variable `var`
#' into a tibble which can be added to the calibration.cal initialization for
#' parameter i.
#'
#' @param cond Character string that defines condition for the variable `var`.
#' @param var Variable to which the condition is applied
#' @param conds_all List of possible values for condition variables ('hsg',
#'   'texture', 'plant', 'landuse').
#'
#' @importFrom dplyr %>%
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
#' @returns A tibble with the columns `NAME` which gives the variable for which
#'   the condition is applied. the column `CHG_TYPE` which gives the Boolean
#'   operator of the condition and `VAL` which is the change value. The names
#'   do not really well represent the columns' meaning but are required for
#'   binding to the rows of calibration.cal.
#'
add_soil_luse <- function(cond, var, cond_all) {
  eval_cond <- paste('cond_all[[var]]', cond) %>%
    parse(text = .) %>%
    eval(.)

  cond <- cond_all[[var]][eval_cond]
  tbl <- tibble(NAME = var,
                CHG_TYPE = '=',
                VAL = 0,
                CONDS = cond)
  return(tbl)
}

#' Add condition lines based on slope
#'
#' The function translates the condition `cond` defined for the slope into a
#' tibble which can be added to the already existing conditions tibble `cond_tbl`.
#'
#' @param cond Character string that defines condition for the variable 'var'
#' @param cond_tbl Tibble with conditions defined based on soil and land use
#'
#' @importFrom dplyr bind_rows %>%
#' @importFrom stringr str_extract str_remove str_sub str_trim
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
#' @returns The conditions table with the slope row added.
#'
add_slope <- function(cond, cond_tbl) {
  cond_op  <- str_extract(cond, '<\\=|\\>\\=|\\=\\=|\\=|\\<|\\>')
  cond_val <- str_remove(cond, cond_op) %>% str_trim(.) %>% as.numeric()
  if(is.na(cond_val)) {
    stop("Parameter condition 'slope ", cond, "' is not allowed.")
  }

  slp_tbl <- tibble(NAME = 'slope',
                    CHG_TYPE = str_sub(cond_op, 1, 1),
                    VAL = cond_val)
  cond_tbl <- bind_rows(cond_tbl, slp_tbl)

  return(cond_tbl)
}

#' Write the updated init files to all parallel folders
#'
#' @param run_path Path to the .model_run folder
#' @param model_setup List with the prepared SWAT+ input settings for the files
#'   `file.cio`, `print.prt`, `time.sim`, and `calibration.cal`.
#'
#' @keywords internal
#'
#'@returns No return value. Writes input text files into the `run_path`.
write_swatplus_setup <- function(run_path, model_setup, run_in_project) {
  if(run_in_project) {
    thread_i <- run_path
  } else {
    thread_i <- dir(run_path, pattern = 'thread_[:0-9:]+', full.names = TRUE)
  }

  ## Write modified file_cio into thread folder and respective Backup folder
  for(i in thread_i) {
    writeLines(model_setup$file.cio,  i%//%"file.cio")
    writeLines(model_setup$time.sim,  i%//%"time.sim")
    writeLines(model_setup$print.prt, i%//%"print.prt")
  }
}
