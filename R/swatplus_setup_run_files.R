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
#' @importFrom lubridate int_end int_start interval yday year years ymd
#' @importFrom dplyr case_when mutate select %>%
#' @importFrom pasta %//% %&%
#' @importFrom purrr map_chr set_names
#' @importFrom readr read_lines read_table
#' @keywords internal
#'
setup_run_files <- function(project_path, parameter, output,
                            start_date, end_date,
                            output_interval, years_skip) {
  ## Read unmodified time.sim, calibration.cal and print.prt
  options(readr.num_columns = 0)
  run_files <- list()
  run_files$time.sim <- read_lines(project_path%//%"time.sim")
  run_files$print.prt <- read_lines(project_path%//%"print.prt")

  print_table <- read_table(project_path%//%"print.prt", skip = 9,
                            col_names = TRUE)

  if(xor(is.null(start_date), is.null(end_date))) {
    stop("'start_date' and 'end_date' must be provided together!")
  } else if (!is.null(start_date)) {
    ## Determine required date indices for writing to time.sim
    time_interval <- interval(ymd(start_date),  ymd(end_date))
    start_year    <- year(int_start(time_interval))
    start_jdn     <- yday(int_start(time_interval))
    end_year      <- year(int_end(time_interval))
    end_jdn       <- yday(int_end(time_interval))

    run_files$time.sim[3] <- c(start_jdn, start_year, end_jdn, end_year, 0) %>%
      sprintf("%10d",.) %>%
      paste(., collapse = "")
  }
  ## Overwrite output interval if value was provided. Default is 'daily'

  output_interval <- substr(output_interval, 1,1) %>% tolower(.)
  output_interval <-
    case_when(output_interval == "d" ~ "daily",
              output_interval == "m" ~ "monthly",
              output_interval == "y" ~ "yearly",
              output_interval == "a" ~ "avann")

  # Set all outputs to no, except the output files defined in output and only
  # for the defined time interval
  object_names <- map_chr(output, ~ .x[["file"]][1])
  print_table[,2:5] <- "n"
  print_table[print_table$objects %in% object_names, output_interval] <- "y"

  print_table <- print_table %>%
    mutate(objects = sprintf("%-16s", objects),
           daily   = sprintf("%14s", daily),
           monthly = sprintf("%14s", monthly),
           yearly  = sprintf("%14s", yearly),
           avann   = sprintf("%14s", avann)) %>%
    apply(., 1, paste, collapse = "")

  run_files$print.prt <- c(run_files$print.prt[1:10], print_table)

  ## Overwrite number of years to skip if value was provided
  if(!is.null(years_skip)) {
    if(!is.numeric(years_skip)) stop("'years_skip' must be numeric!")
    years_skip <- sprintf("%-12d", years_skip)
    run_files$print.prt[3] <- run_files$print.prt[3] %>%
      strsplit(., "\\s+") %>%
      unlist(.) %>%
      .[2:length(.)] %>%
      sprintf("%-10s",. ) %>%
      c(years_skip, .) %>%
      paste(., collapse = "")
  }

  # So far avoid any other output files to be written
  run_files$print.prt[7] <- "n             n             n             "
  run_files$print.prt[7] <- "n             n             n             n             "

  # Current implementation of parameter calibration does not allow any constraints!
  if(!is.null(parameter)) {
    run_files$calibration.cal <- parameter$parameter_constrain %>%
      select(., parameter, change) %>%
      set_names(c("NAME", "CHG_TYPE")) %>%
      mutate(VAL = NA, CONDS = 0, LYR1 = 0, LYR2 = 0, YEAR1 = 0, YEAR2 = 0,
             DAY1 = 0, DAY2 = 0, OBJ_TOT = 0)
  }

  return(run_files)
}

#' Write the updated file.cio to all parallel folders
#'
#' @param run_path Path to the .model_run folder
#' @param file_cio Updated file_cio to be written
#'
#' @keywords internal
#'
write_run_files <- function(run_path, run_files) {
  thread_i <- dir(run_path) %>%
    substr(.,(nchar(.) - 7), nchar(.)) %>%
    .[grepl("thread_",.)]
  ## Write modified file_cio into thread folder and respective Backup folder
  for(i in thread_i) {
    writeLines(run_files$time.sim, run_path%//%i%//%"time.sim")
    writeLines(run_files$print.prt, run_path%//%i%//%"print.prt")
  }
}
