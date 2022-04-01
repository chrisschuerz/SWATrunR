#' Reads and modifies the SWAT+ projects' files that define the basic settings
#' of the simulation and set them according tu respective input parameters
#'
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#' @param start_date Start date of the SWAT simulation. Provided as character
#'   string in any ymd format (e.g. 'yyyy-mm-dd') or in Date format project are
#'   located
#' @param end_date End date of the SWAT simulation. Provided as character string
#'   in any ymd format (e.g. 'yyyy-mm-dd') or in Date format project are located
#' @param output_interval Time interval in which the SWAT model outputs are
#'   written. Provided either as character string ("d" for daily, "m" for
#'   monthly, "y" for yearly, or "a", for average annual).
#' @param years_skip Integer value that provides the numbe of years to be
#'   skipped during writing the SWAT model outputs
#' @importFrom lubridate as_date ceiling_date int_end int_start interval
#'   leap_year yday year years ymd
#' @importFrom dplyr case_when mutate select %>%
#' @importFrom purrr map_chr set_names
#' @importFrom readr read_lines read_table
#' @importFrom stringr str_sub
#' @keywords internal
#'
setup_swatplus <- function(project_path, parameter, output,
                           start_date, end_date, start_date_print,
                           output_interval, years_skip, unit_cons) {
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
    map_df(., ~tibble(objects = .x[1], daily = .x[2],
                      monthly = .x[3], yearly = .x[3], avann = .x[4]))

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
    print_jdn  <- max(print_time[2], 1)
    print_year <- max(print_time[3], year(start_date))
    start_date_print <- as_date(x = print_year%//%print_jdn, format = "%Y/%j")

  } else if(!is.null(start_date_print)){
    start_date_print <- ymd(start_date_print)
    if(start_date_print < start_date) {
      stop("'start_date_print' is an earlier date than 'start_date'!")
    }
    years_skip <- year(start_date_print) - year(start_date)
    print_jdn  <- yday(start_date_print)
    print_year <- year(start_date_print)

  } else {
    print_jdn  <- 0
    print_year <- 0
    start_date_print <- start_date + years_skip
  }

  model_setup$print.prt[3] <-
    c(years_skip, print_jdn, print_year, 0, 0, 1) %>%
    sprintf("%-11d", .) %>%
    paste(., collapse = '')

  years_sim <- interval(start_date, end_date)/years(1)
  if(years_skip >= years_sim) {
    stop("Defined simulation period is not longer than the number of years to skip!")
  }

  model_setup$years_skip <- as.numeric(years_skip)
  model_setup$start_date_print <- start_date_print

  ## Output interval settings
  ## Set output_interval to 'daily' as default if not provided by user.
  if(is.null(output_interval)) output_interval <- "d"

  if(output_interval == 'm') {
    t_int <- seq(start_date_print, end_date, by = 'm')
    if(length(t_int) == 1 & end_date != ceiling_date(end_date, unit = 'm')-1) {
      stop('Monthly simulation outputs require a simulation period of ',
           'at least one full month!')
    }
  }

  output_interval <- str_sub(output_interval, 1,1) %>% tolower(.)
  output_interval <-
    case_when(output_interval == "d" ~ "daily",
              output_interval == "m" ~ "monthly",
              output_interval == "y" ~ "yearly",
              output_interval == "a" ~ "avann")

  model_setup$output_interval <- output_interval

  # Set all outputs to no, except the output files defined in output and only
  # for the defined time interval
  object_names <- unique(output$file)
  print_table[,2:5] <- "n"
  print_table[print_table$objects %in% object_names, output_interval] <- "y"

  print_table <- print_table %>%
    mutate(objects = sprintf("%-16s", objects),
           daily   = sprintf("%14s", daily),
           monthly = sprintf("%14s", monthly),
           yearly  = sprintf("%14s", yearly),
           avann   = sprintf("%14s", avann)) %>%
    apply(., 1, paste, collapse = "")

  model_setup$print.prt <- c(model_setup$print.prt[1:10], print_table)

  # So far avoid any other output files to be written
  model_setup$print.prt[7] <- "n             n             n             "
  model_setup$print.prt[9] <- "n             n             n             n             "

  if(!is.null(parameter)) {
    model_setup$calibration.cal <- map(1:nrow(parameter$definition), ~ parameter$definition[.x,]) %>%
      map(., ~ setup_calibration_cal(.x, unit_cons)) %>%
      bind_rows(.)
  }

  return(model_setup)
}

#' Setup the calibration.cal file and include all parameter conditions
#' of the simulation and set them according to respective input parameters
#'
#' @param par_def Tibble with one line that includes the parameter definition of
#'   parameter i
#' @param unit_cons List of tibbles that contains the meta information of hru, aqu,
#'   etc. units and constrain variables (e.g. texture, plant)
#'
#' @importFrom dplyr bind_rows select %>%
#' @importFrom purrr map map2_df
#' @importFrom tibble tibble
#' @importFrom tidyselect any_of
#'
#' @keywords internal
#'
setup_calibration_cal <- function(par_def, unit_cons) {
  par_cal <- init_cal(par_def)

  if('lyr' %in% names(par_def)) {
    if(!is.na(par_def$lyr)) {
      par_cal <- add_value_range(par_cal, par_def$lyr, 'LYR')
    }
  }
  if('year' %in% names(par_def)) {
    if(!is.na(par_def$year)) {
      par_cal <- add_value_range(par_cal, par_def$year, 'YEAR')
    }
  }
  if('day' %in% names(par_def)) {
    if(!is.na(par_def$day)) {
      par_cal <- add_value_range(par_cal, par_def$day, 'DAY')
    }
  }
  if('unit' %in% names(par_def)) {
    if(!is.na(par_def$unit)) {
      par_cal <- add_obj(par_cal, par_def$unit,
                         unit_cons$units[[par_def$file_name]])
    }
  }

  if(any(c('hsg', 'texture', 'plant', 'landuse') %in% names(par_def))) {
      soil_luse <- select(par_def, any_of(c('hsg', 'texture', 'plant', 'landuse'))) %>%
        select(., !tidyselect:::where(is.na)) %>% # will be replaced when where is in the tidyselect namespace
        map(., ~ .x)
      cond_tbl <- map2_df(soil_luse, names(soil_luse), ~ add_soil_luse(.x, .y, unit_cons$conds))
  } else {
    cond_tbl <- tibble()
  }

  if(any('slope' %in% names(par_def))) {
    if(!is.na(par_def$slope)) {

      cond_tbl <- add_slope(par_def$slope, cond_tbl)
    }
  }

  if(any(c('hsg', 'texture', 'plant', 'landuse', 'slope') %in% names(par_def))) {
    par_cal$CONDS <- as.character(nrow(cond_tbl))
    par_cal <- bind_rows(par_cal, cond_tbl)
  }

  return(par_cal)
}

#' Initialize the calibration.cal table
#'
#' @param par_def Tibble with one line that includes the parameter definition of
#'   parameter i
#' @importFrom dplyr mutate select %>%
#' @importFrom purrr set_names
#' @keywords internal
#'
init_cal <- function(par_def) {
  par_def %>%
  select(., parameter, change) %>%
    set_names(c("NAME", "CHG_TYPE")) %>%
    mutate(VAL = NA_real_, CONDS = '0', LYR1 = 0, LYR2 = 0, YEAR1 = 0, YEAR2 = 0,
           DAY1 = 0, DAY2 = 0, OBJ_TOT = 0)
}

#' Add the value range 'val' for the condition variable 'var' for a parameter to
#' calibration.cal
#'
#' @param par_cal The calibration.cal tibble for the parameter i
#' @param val Vector that defines the value range
#' @param var Character string. Name of the variable
#'
#' @keywords internal
#'
add_value_range <- function(par_cal, val, var) {
  val_range <- get_value_range(val)
  par_cal[var%&%'1'] <- val_range[1]
  par_cal[var%&%'2'] <- val_range[2]
  return(par_cal)
}

#' Get the value range from a condition in the par_def table
#'
#' @param condition Character string that defines a condition
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_detect str_remove
#' @importFrom purrr set_names
#' @keywords internal
#'
get_value_range <- function(condition) {
  if(str_detect(condition, '%in%')) {
    condition <- condition %>%
      str_remove(., '%in%') %>%
      parse(text = .) %>%
      eval(.)
  } else if (str_detect(condition, '==')) {
    condition <- condition %>%
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
#' @param par_cal The calibration.cal tibble for the parameter i
#' @param unit Character string that defines condition for the object units (ids of e.g. hru objects)
#' @param unit_all List of vectors that define the unit ids of all object types.
#'
#' @importFrom dplyr bind_cols bind_rows %>%
#' @importFrom purrr set_names
#'
#' @keywords internal
#'
add_obj <- function(par_cal, unit, unit_all) {
  eval_unit <- paste('unit_all', unit) %>%
  parse(text = .) %>%
  eval(.)

  unit <- unit_all[eval_unit] %>%
    unique(.) %>%
    sort(.) %>%
    identify_sequence(.) %>%
    set_names(., 'OBJ'%_%1:length(.)) %>%
    bind_rows()

  par_cal$OBJ_TOT <- length(unit)
  par_cal <- bind_cols(par_cal, unit)

  return(par_cal)
}

#' Group sequences of units together for writing the OBJ columns in calibration.cal
#'
#' @param val Numeric vector with the unit values
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map map2 reduce
#'
#' @keywords internal
#'
identify_sequence <- function(val) {
  split_end <- which(diff(val) > 1)
  split_start <- c(1, split_end + 1)
  split_end <- c(split_end, length(val))
  map2(split_start, split_end, ~ val[.x:.y]) %>%
  map(., ~ translate_sequence(.x)) %>%
  reduce(., c)
}

#' Translate the unit sequences to calibration.cal syntax
#'
#' @param val_seq Numeric vector with the unit values
#'
#' @keywords internal
#'
translate_sequence <- function(val_seq) {
  if (length(val_seq) > 1) {
    val_seq <- c(min(val_seq), -max(val_seq))
  }
  return(val_seq)
}

#' Add condition lines based on soil and land use variables
#'
#' @param cond Character string that defines condition for the variable 'var'
#' @param var Character string. Variable for which condition is applied
#' @param cond_all List of vectors that define all possible values for the
#'   condition variables.
#'
#' @importFrom dplyr %>%
#' @importFrom tibble tibble
#'
#' @keywords internal
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
#' @param cond Character string that defines condition for the variable 'var'
#' @param cond_tbl Tibble with conditions defined based on soil and land use
#'
#' @importFrom dplyr bind_rows %>%
#' @importFrom stringr str_extract str_remove str_sub str_trim
#' @importFrom tibble tibble
#'
#' @keywords internal
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
#' @param model_setup List of files that define the SWAT+ model setup
#'
#' @keywords internal
#'
write_swatplus_setup <- function(run_path, model_setup) {
  thread_i <- dir(run_path) %>%
    .[. %in% ("thread"%_%1:999)]

  ## Write modified file_cio into thread folder and respective Backup folder
  for(i in thread_i) {
    writeLines(model_setup$file.cio, run_path%//%i%//%"file.cio")
    writeLines(model_setup$time.sim, run_path%//%i%//%"time.sim")
    writeLines(model_setup$print.prt, run_path%//%i%//%"print.prt")
  }
}
