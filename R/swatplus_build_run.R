#' Generate folder structure for parallel SWAT execution
#'
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#' @param run_path Path where the '.model_run' folder is built. If NULL the
#'   executable model is built in the 'project_path'
#' @param n_thread Number of parallel threads that will be created. This number
#'   must be in accordance to the number of cores of the PC
#' @param file_cio The file_cio from the 'project_folder' modified according to
#'   'start_date', 'end_date', 'output_interval', and 'years_skip'
#' @param abs_swat_val Replace internal Absolute_SWAT_Values.txt' file with
#'   custom one found in this path if provided
#'
#' @importFrom parallel detectCores
#' @importFrom dplyr %>%
#' @importFrom lubridate now
#' @importFrom pasta %//% %_% %&&%
#' @keywords internal
#'

build_model_run <- function(project_path, run_path, n_thread,
                            abs_swat_val, quiet){
  # Identify operating system and find the SWAT executable in the project folder
  os <- get_os()
  if(os == "win") {
    swat_exe <- list.files(project_path) %>%
      .[grepl(".exe$",.)]
  } else if(os == "unix") {
    swat_exe <- system("find"%&&%project_path%&&%"-executable -type f",
                       intern = TRUE) %>%
      basename(.)
  }

  # Make shure that there is exactly one executable in the SWAT project folder
  if(length(swat_exe) == 0) stop("No SWAT executable found in the project folder!")
  if(length(swat_exe) > 1) stop("Project folter contains more than one executable!")

  # Batch file template
  batch_temp <- list(win = c("@echo off",
                             substr(run_path, 1, 2),
                             "cd"%&&%run_path,
                             swat_exe,
                             "if %errorlevel% == 0 exit 0",
                             "echo."),
                     unix = NULL) #Required batch for running SWAT in Linux coming soon

  if(!quiet) {
    plural <- ifelse(n_thread == 1, "", "s")
    cat("Building", n_thread, "thread"%&%plural, "in","'"%&%run_path%&%"':", "\n")
  }

  # Create folder structure to execute SWAT
  if(os == "win") {
    swat_files <- dir(project_path, full.names = TRUE)
    ## To save storage do not copy allready existing output files
    swat_files <- swat_files[!grepl("output.hru|output.pst|output.rch|output.rsv|output.sed|output.std|output.sub",swat_files)]
    dir.create(run_path, recursive = TRUE)
    t0 <- now()
    for (i in 1:n_thread){
      ## Copy all files from the project folder to the respective thread
      dir.create(run_path%//%"thread"%_%i)
      file.copy(swat_files, run_path%//%"thread"%_%i)

      ## Write the batch file that will be executed to call the SWAT exe when
      ## executing SWAT in a later step
      swat_bat <- batch_temp[[os]]
      swat_bat[3] <- swat_bat[3]%//%"thread"%_%i
      writeLines(swat_bat, con = run_path%//%"thread"%_%i%//%"swat_run.bat")

      if(!quiet) {
        display_progress(i, n_thread, t0, "Thread")
      }
    }
    if(!quiet){
      finish_progress(n_thread, t0, "thread")
    }
  }
}

#' Identify the OS (provided by Gabor Csardi)
#' @keywords internal
#'

get_os <- function() {
  if (.Platform$OS.type == "windows") {
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    #"mac"
    stop("SWATplusR only supported for Windows and Linux")
  } else if (.Platform$OS.type == "unix") {
    #"unix"
    stop("unix Version under development!")
  } else {
    stop("Unknown OS")
  }
}

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
setup_run_files <- function(project_path, parameter, output,
                            start_date, end_date,
                            output_interval, years_skip) {
  ## Read unmodified time.sim, calibration.cal and print.prt
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
  if(is.null(output_interval)) output_interval <- "d"
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
write_file_cio <- function(run_path, file_cio) {
  thread_i <- dir(run_path) %>%
    substr(.,(nchar(.) - 7), nchar(.)) %>%
    .[grepl("thread_",.)]
  ## Write modified file_cio into thread folder and respective Backup folder
  for(i in thread_i) {
    writeLines(file_cio, run_path%//%i%//%"file.cio")
  }
}
