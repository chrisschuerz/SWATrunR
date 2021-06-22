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
#' @importFrom lubridate as_date int_end int_start interval leap_year yday year
#'   years ymd
#' @importFrom dplyr case_when mutate select %>%
#' @importFrom purrr map_chr set_names
#' @importFrom readr read_lines read_table
#' @importFrom stringr str_sub
#' @keywords internal
#'
setup_swatplus <- function(project_path, parameter, output,
                           start_date, end_date,
                           output_interval, years_skip,
                           soft_cal, unit_cons) {
  ## Read unmodified time.sim, calibration.cal and print.prt
  options(readr.num_columns = 0)
  model_setup <- list()
  model_setup$time.sim <- read_lines(project_path%//%"time.sim")
  model_setup$print.prt <- read_lines(project_path%//%"print.prt")

  print_table <- read_table(project_path%//%"print.prt", skip = 9,
                            col_names = TRUE) %>%
    set_names(., tolower(colnames(.)))

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
  if(is.null(years_skip)) {
    years_skip <- model_setup$print.prt[3] %>%
      strsplit(., "\\s+") %>%
      unlist(.) %>%
      .[nchar(.) > 0] %>%
      .[1] %>%
      as.numeric(.)
  } else {
    if(!is.numeric(years_skip)) stop("'years_skip' must be numeric!")
    model_setup$print.prt[3] <- model_setup$print.prt[3] %>%
      strsplit(., "\\s+") %>%
      unlist(.) %>%
      .[nchar(.) > 0] %>%
      .[2:length(.)] %>%
      sprintf("%-10s",. ) %>%
      c(sprintf("%-12d", years_skip), .) %>%
      paste(., collapse = "")
  }

  years_sim <- interval(start_date, end_date)/years(1)
  if(years_skip >= years_sim) {
    stop("Defined simulation period is not longer than the number of years to skip!")
  }

  model_setup$years_skip <- as.numeric(years_skip)

  ## Output interval settings
  ## Set output_interval to 'daily' as default if not provided by user.
  if(is.null(output_interval)) output_interval <- "d"

  output_interval <- str_sub(output_interval, 1,1) %>% tolower(.)
  output_interval <-
    case_when(output_interval == "d" ~ "daily",
              output_interval == "m" ~ "monthly",
              output_interval == "y" ~ "yearly",
              output_interval == "a" ~ "avann")

  model_setup$output_interval <- output_interval

  # Set all outputs to no, except the output files defined in output and only
  # for the defined time interval
  object_names <- map_chr(output, ~ .x[["file"]][1])
  print_table[,2:5] <- "n"
  print_table[print_table$objects %in% object_names, output_interval] <- "y"

  if(soft_cal) {
    print_table[print_table$objects %in% c("basin_wb", "basin_nb"),
                "avann"] <- "y"
  }

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
  model_setup$print.prt[7] <- "n             n             n             n             "

  # Current implementation of parameter calibration does not allow any constraints!
  if(!is.null(parameter)) {
    model_setup$calibration.cal <- map(1:nrow(parameter$definition), ~ parameter$definition[.x,]) %>%
      map(., ~ setup_calibration_cal(.x, unit_cons)) %>%
      bind_rows(.)
  }

  return(model_setup)
}

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
        select(., !where(is.na)) %>%
        map(., ~ .x)
      cond_tbl <- map2_df(soil_luse, names(soil_luse), ~ add_soil_luse(.x, .y, unit_cons$conds))
  }

  if(any('slope' %in% names(par_def))) {
    if(!is.na(par_def$slope)) {

      cond_tbl <- add_slope(par_def$slope, cond_tbl)
    }
  }

  if(any(c('hsg', 'texture', 'plant', 'landuse') %in% names(par_def))) {
    par_cal$CONDS <- as.character(nrow(cond_tbl))
    par_cal <- bind_rows(par_cal, cond_tbl)
  }

  return(par_cal)
}

init_cal <- function(par_def) {
  par_def %>%
  select(., parameter, change) %>%
    set_names(c("NAME", "CHG_TYPE")) %>%
    mutate(VAL = NA_real_, CONDS = '0', LYR1 = 0, LYR2 = 0, YEAR1 = 0, YEAR2 = 0,
           DAY1 = 0, DAY2 = 0, OBJ_TOT = 0)
}

add_value_range <- function(par_cal, val, var) {
  val_range <- get_value_range(val)
  par_cal[var%&%'1'] <- val_range[1]
  par_cal[var%&%'2'] <- val_range[2]
  return(par_cal)
}

# add_year <- function(par_cal, yr) {
#   yr_range <- get_value_range(yr)
#   par_cal$YEAR1 <- yr_range[1]
#   par_cal$YEAR2 <- yr_range[2]
#   return(par_cal)
# }
#
# add_day <- function(par_cal, day) {
#   day_range <- get_value_range(day)
#   par_cal$DAY1 <- day_range[1]
#   par_cal$DAY2 <- day_range[2]
#   return(par_cal)
# }

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

identify_sequence <- function(val) {
  split_end <- which(diff(val) > 1)
  split_start <- c(1, split_end + 1)
  split_end <- c(split_end, length(val))
  map2(split_start, split_end, ~ val[.x:.y]) %>%
  map(., ~ translate_sequence(.x)) %>%
  reduce(., c)
}

translate_sequence <- function(val_seq) {
  if (length(val_seq) > 1) {
    val_seq <- c(min(val_seq), -max(val_seq))
  }
  return(val_seq)
}


#' Write the updated file.cio to all parallel folders
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
    writeLines(model_setup$time.sim, run_path%//%i%//%"time.sim")
    writeLines(model_setup$print.prt, run_path%//%i%//%"print.prt")
  }
}
