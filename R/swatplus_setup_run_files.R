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
#' @keywords internal
#'
setup_swatplus <- function(project_path, parameter, output,
                           start_date, end_date,
                           output_interval, years_skip,
                           soft_cal) {
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

    start_date <- as_date(x = start_year%//%start_day,
                          tz ="UTC", format = "%Y/%j")
    end_date   <- as_date(x = end_year%//%end_day,
                          tz ="UTC", format = "%Y/%j")
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

  output_interval <- substr(output_interval, 1,1) %>% tolower(.)
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
    model_setup$calibration.cal <- parameter$definition %>%
      select(., parameter, change) %>%
      set_names(c("NAME", "CHG_TYPE")) %>%
      mutate(VAL = NA, CONDS = 0, LYR1 = 0, LYR2 = 0, YEAR1 = 0, YEAR2 = 0,
             DAY1 = 0, DAY2 = 0, OBJ_TOT = 0)
  }

  return(model_setup)
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
