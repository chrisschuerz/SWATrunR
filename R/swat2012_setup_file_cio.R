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
#' @importFrom lubridate int_end int_start interval yday year years ymd
#' @importFrom dplyr case_when %>%
#' @importFrom pasta %//% %&%
#' @keywords internal
#'
modify_file_cio <- function(project_path, start_date, end_date,
                            output_interval, years_skip,
                            rch_out_var, sub_out_var,
                            hru_out_var, hru_out_nr) {
  ## Read unmodified file.cio
  file_cio <- readLines(project_path%//%"file.cio", warn = FALSE)

  if(xor(is.null(start_date), is.null(end_date))) {
    stop("'start_date' and 'end_date' must be provided together!")
  } else if (!is.null(start_date)) {
    ## Determine required date indices for writing to file.cio
    time_interval <- interval(ymd(start_date),  ymd(end_date))
    n_year        <- ceiling(time_interval / years(1))
    start_year    <- year(int_start(time_interval))
    start_jdn     <- yday(int_start(time_interval))
    end_jdn       <- yday(int_end(time_interval))

    file_cio[8]  <- sprintf("%16d", n_year)%&%    "    | NBYR : Number of years simulated"
    file_cio[9]  <- sprintf("%16d", start_year)%&%"    | IYR : Beginning year of simulation"
    file_cio[10] <- sprintf("%16d", start_jdn)%&% "    | IDAF : Beginning julian day of simulation"
    file_cio[11] <- sprintf("%16d", end_jdn)%&%   "    | IDAL : Ending julian day of simulation"
  }
  ## Overwrite output interval if value was provided
  if(!is.null(output_interval)){
    output_interval <- substr(output_interval, 1,1) %>% tolower(.)
    output_interval <- case_when(output_interval %in% c("m", "0") ~ 0,
                                 output_interval %in% c("d", "1") ~ 1,
                                 output_interval %in% c("y", "2") ~ 2)
    file_cio[59] <- sprintf("%16d", output_interval)%&%"    | IPRINT: print code (month, day, year)"
  }

  ## Overwrite number of years to skip if value was provided
  if(!is.null(years_skip)) {
    if(!is.numeric(years_skip)) stop("'years_skip' must be numeric!")
    file_cio[60] <- sprintf("%16d", years_skip)%&%"    | NYSKIP: number of years to skip output printing/summarization"
  }

  ## Overwrite custom reach variables if values are provided
  if(!is.null(rch_out_var)){
    if(!is.numeric(rch_out_var)) stop("'rch_out_var' must be numeric!")
    if(length(rch_out_var) > 20){
      stop("Maximum number of reach variables for custom outputs in file.cio is 20!")
    }
    rch_out_var <- c(rch_out_var, rep(0, 20 - length(rch_out_var)))
    file_cio[65] <- paste0(sprintf("%4d", rch_out_var), collapse = "")
  }

  ## Overwrite custom subbasin variables if values are provided
  if(!is.null(sub_out_var)){
    if(!is.numeric(sub_out_var)) stop("'sub_out_var' must be numeric!")
    if(length(sub_out_var) > 15){
      stop("Maximum number of subbasin variables for custom outputs in file.cio is 15!")
    }
    sub_out_var <- c(sub_out_var, rep(0, 15 - length(sub_out_var)))
    file_cio[67] <- paste0(sprintf("%4d", sub_out_var), collapse = "")
  }

  ## Overwrite custom HRU variables if values are provided
  if(!is.null(hru_out_var)){
    if(!is.numeric(hru_out_var)) stop("'hru_out_var' must be numeric!")
    if(length(hru_out_var) > 20){
      stop("Maximum number of HRU variables for custom outputs in file.cio is 20!")
    }
    hru_out_var <- c(hru_out_var, rep(0, 20 - length(hru_out_var)))
    file_cio[69] <- paste0(sprintf("%4d", hru_out_var), collapse = "")
  }

  ## Overwrite HRU numbers for which HRU outputs are written if values are provided
  if(!is.null(hru_out_nr)){
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
    file_cio[71] <- paste0(sprintf("%4d", hru_out_nr), collapse = "")
  }
  return(file_cio)
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
    substr(.,(nchar(.) - 7), nchar(.)) %>%
    .[grepl("thread_",.)]
  ## Write modified file_cio into thread folder and respective Backup folder
  for(i in thread_i) {
    writeLines(file_cio, run_path%//%i%//%"file.cio")
  }
}
