#' Reads and modifies the SWAT projects' filo.cio according to provided inputs
#'
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#' @param start_date Start date of the SWAT simulation. Provided as character
#'   string in any ymd format (e.g. 'yyyy-mm-dd') or in Date format project are
#'   located
#' @param end_date End date of the SWAT simulation. Provided as character string
#'   in any ymd format (e.g. 'yyyy-mm-dd') or in Date format project are located
#' @param output_interval Time interval in which the SWAT model outputs are
#'   written. Provided either as character string ("d" for daily, "m" for
#'   monthly, or "y" for yearly) or as SWAT input values (0 for monthly, 1 for
#'   daily, 2 for yearly).
#' @param years_skip Integer value that provides the numbe of years to be
#'   skipped during writing the SWAT model outputs
#' @param rch_out_var Numeric vector of maximum length = 20 for customized
#'   output of reach variables.For output codes see
#'   \href{https://swat.tamu.edu/media/69308/ch03_input_cio.pdf}{SWAT I/O
#'   Documentation} p.77ff.
#' @param sub_out_var Numeric vector of maximum length = 15 for customized
#'   output of subbasin variables.For output codes see
#'   \href{https://swat.tamu.edu/media/69308/ch03_input_cio.pdf}{SWAT I/O
#'   Documentation} p.78ff.
#' @param hru_out_var Numeric vector of maximum length = 20 for customized
#'   output of HRU variables.For output codes see
#'   \href{https://swat.tamu.edu/media/69308/ch03_input_cio.pdf}{SWAT I/O
#'   Documentation} p.79ff.
#' @param hru_out_nr Numeric vector of maximum length = 20 for providing the HRU
#'   numbers for which the HRU variables are written. Optional if hru_out_nr =
#'   'all', HRU variables are written for all HRU (caution, very large output
#'   files possible!)
#' @importFrom dplyr case_when mutate select %>%
#' @importFrom lubridate as_date floor_date int_end int_start interval yday year years ymd
#' @importFrom purrr map map_chr map_dbl map_df set_names
#' @importFrom readr read_lines read_table
#' @importFrom stringr str_remove str_sub
#' @keywords internal
#'
setup_swat2012 <- function(project_path, output,
                           start_date, end_date,
                           output_interval, years_skip,
                           rch_out_var, sub_out_var,
                           hru_out_var, hru_out_nr) {
  model_setup <- list()

  ## Fix unix/win case issue with file "Tmp1.tmp"
    suppressWarnings(file.rename(project_path%//%"Tmp1.Tmp",
                                 project_path%//%"tmp1.tmp"))

  ## Read unmodified file.cio
  file_cio <- readLines(project_path%//%"file.cio", warn = FALSE)

  if(xor(is.null(start_date), is.null(end_date))) {
    stop("'start_date' and 'end_date' must be provided together!")
  } else if (!is.null(start_date)) {
    ## Determine required date indices for writing to file.cio
    start_date <- ymd(start_date)
    end_date   <- ymd(end_date)
    time_interval <- interval(start_date,  end_date)
    n_year        <- ceiling(time_interval / years(1))
    start_year    <- year(int_start(time_interval))
    start_jdn     <- yday(int_start(time_interval))
    end_jdn       <- yday(int_end(time_interval))

    file_cio[8]  <- sprintf("%16d", n_year)%&%    "    | NBYR : Number of years simulated"
    file_cio[9]  <- sprintf("%16d", start_year)%&%"    | IYR : Beginning year of simulation"
    file_cio[10] <- sprintf("%16d", start_jdn)%&% "    | IDAF : Beginning julian day of simulation"
    file_cio[11] <- sprintf("%16d", end_jdn)%&%   "    | IDAL : Ending julian day of simulation"
  } else {
    n_year     <- cio_to_numeric(file_cio[8])
    start_year <- cio_to_numeric(file_cio[9])
    end_year   <- start_year + n_year - 1
    start_jdn  <- cio_to_numeric(file_cio[10])
    end_jdn    <- cio_to_numeric(file_cio[11])

    start_date <- as_date(x = start_year%//%start_jdn, format = "%Y/%j")
    end_date   <- as_date(x = end_year%//%end_jdn, format = "%Y/%j")
  }

  model_setup$start_date <- start_date
  model_setup$end_date   <- end_date

  ## Check the numbers of years to skip with the simulation interval Overwrite
  ## number of years to skip if value was provided otherwise get value from
  ## print file
  if(is.null(years_skip)) {
    years_skip <- cio_to_numeric(file_cio[60])
  } else {
    if(!is.numeric(years_skip)) stop("'years_skip' must be numeric!")
    file_cio[60] <- sprintf("%16d", years_skip)%&%"    | NYSKIP: number of years to skip output printing/summarization"
  }

  years_sim <- interval(start_date, end_date)/years(1)
  if(years_skip >= years_sim) {
    stop("Defined simulation period is not longer than the number of years to skip!")
  }

  model_setup$years_skip <- as.numeric(years_skip)

  ## Output interval settings
  ## Overwrite output interval if value was provided
  if(is.null(output_interval)){
    output_interval <- cio_to_numeric(file_cio[59])
    output_interval <- case_when(output_interval == 0 ~ "m",
                                 output_interval == 1 ~ "d",
                                 output_interval == 2 ~ "y")
  } else {
    output_interval <- str_sub(output_interval, 1,1) %>% tolower(.)
    out_int <- case_when(output_interval %in% c("m", "0") ~ 0,
                         output_interval %in% c("d", "1") ~ 1,
                         output_interval %in% c("y", "2") ~ 2)
    file_cio[59] <- sprintf("%16d", out_int)%&%"    | IPRINT: print code (month, day, year)"
  }

  model_setup$output_interval <- output_interval

  ## Number of records that should be in output

  if(output_interval %in% c("d", "m", "y")) {
    n_row <- seq(start_date, end_date, by = output_interval) %>%
      floor_date(., unit = output_interval) %>%
      length(.)
  } else {
    n_row <- 1
  }

  ## Overwrite custom reach variables if values are provided
  if(!is.null(rch_out_var)){
    if(!is.numeric(rch_out_var)) stop("'rch_out_var' must be numeric!")
    if(length(rch_out_var) > 20){
      stop("Maximum number of reach variables for custom outputs in file.cio is 20!")
    }
  }
  rch_out_var <- c(rch_out_var, rep(0, 20 - length(rch_out_var)))
  file_cio[65] <- paste0(sprintf("%4d", rch_out_var), collapse = "")

  ## Overwrite custom subbasin variables if values are provided
  if(!is.null(sub_out_var)){
    if(!is.numeric(sub_out_var)) stop("'sub_out_var' must be numeric!")
    if(length(sub_out_var) > 15){
      stop("Maximum number of subbasin variables for custom outputs in file.cio is 15!")
    }
  }
  sub_out_var <- c(sub_out_var, rep(0, 15 - length(sub_out_var)))
  file_cio[67] <- paste0(sprintf("%4d", sub_out_var), collapse = "")

  ## Consider the case of HRU output definition and the setting of the HRUs in file.cio
  if(("output.hru" %in% output$file)) {
    hru_idx <- output %>%
      filter(file == "output.hru") %>%
      select(unit) %>%
      unlist(.) %>%
      unique(.)

    if(!is.null(hru_out_nr)) {
      warning("HRU output definition overwrites 'hru_out_var'!")
    }

    if(length(hru_idx) <= 20) {
      hru_out_nr <- hru_idx
    } else {
      hru_out_nr <- "all"
    }

  } else if (all(c(is.null(hru_out_nr), is.null(hru_out_var)))) {
    hru_out_var <- 1
    hru_out_nr  <- 1
  }

  ## Overwrite custom HRU variables if values are provided
  if(!is.null(hru_out_var)){
    if(!is.numeric(hru_out_var)) stop("'hru_out_var' must be numeric!")
    if(length(hru_out_var) > 20){
      stop("Maximum number of HRU variables for custom outputs in file.cio is 20!")
    }
  }
  hru_out_var <- c(hru_out_var, rep(0, 20 - length(hru_out_var)))
  file_cio[69] <- paste0(sprintf("%4d", hru_out_var), collapse = "")


  ## Overwrite HRU numbers for which HRU outputs are written if values are provided
  if(is.null(hru_out_nr)){
    hru_out_nr <- rep(0, 20)
  } else {
    if(is.numeric(hru_out_nr)){
      if(length(hru_out_nr) > 20){
        stop("Maximum number of HRUs for custom outputs in file.cio is 20!")
      }
      hru_out_nr <- c(hru_out_nr, rep(0, 20 - length(hru_out_nr)))
    } else if(hru_out_nr == "all") {
      hru_out_nr <- rep(0, 20)
    } else {
      stop("Input for 'hru_out_nr' must be either numeric vector or string 'all'!")
    }
  }
  file_cio[71] <- paste0(sprintf("%4d", hru_out_nr), collapse = "")

  model_setup$file.cio <- file_cio

  return(model_setup)
}

#' Write the updated file.cio to all parallel folders
#'
#' @param run_path Path to the .model_run folder
#' @param file_cio Updated file_cio to be written
#'
#' @keywords internal
#'
write_file_cio <- function(run_path, file_cio) {
  thread_i <- dir(run_path) %>%
    .[. %in% ("thread"%_%1:999)]
  ## Write modified file_cio into thread folder and respective Backup folder
  for(i in thread_i) {
    writeLines(file_cio, run_path%//%i%//%"file.cio")
  }
}
