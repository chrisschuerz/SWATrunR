#' Generate folder structure for parallel SWAT execution
#'
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#' @param project_path Path where the '.model_run' folder is built. If NULL the
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
#' @importFrom pasta %//% %_% %&&%
#' @keywords internal
#'

build_model_run <- function(project_path, run_path, n_thread,
                            file_cio, abs_swat_val){
  # If run_path is not provided '.model_run' is built directly in 'project_path'
  if(is.null(run_path)) run_path <- project_path

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

  # Create folder structure to execute SWAT
  if(os == "win") {
    print("Building folder '.model_run' for the SWAT model execution:")
    swat_files <- dir(project_path, full.names = TRUE)
    ## To save storage do not copy allready existing output files
    swat_files <- swat_files[!grepl("output.hru|output.pst|output.rch|output.rsv|output.sed|output.std|output.sub",swat_files)]
    dir.create(run_path%//%".model_run")
    pb <- progress_estimated(n_thread)
    for (i in 1:n_thread){
      pb$begin()$print()

      ## Copy all files from the project folder to the respective thread
      dir.create(run_path%//%".model_run"%//%"thread"%_%i)
      file.copy(swat_files, run_path%//%".model_run"%//%"thread"%_%i)

      ## Create a Backup folder and copy files to there as well
      ## Required for rewriting parameters with Swat_edit.exe
      dir.create(run_path%//%".model_run"%//%"thread"%_%i%//%"Backup")
      file.copy(swat_files,
                run_path%//%".model_run"%//%"thread"%_%i%//%"Backup")

      ## Write all files required for rewriting parameters using the
      ## Swat_edit.exe from SWAT CUP
      file.copy(system.file("extdata", "Swat_Edit.exe",
                            package = "SWATplusR"),
                run_path%//%".model_run"%//%"thread"%_%i)
      file.copy(system.file("extdata", "SUFI2_execute.exe",
                            package = "SWATplusR"),
                run_path%//%".model_run"%//%"thread"%_%i)
      if(is.null(abs_swat_val)) {
        file.copy(system.file("extdata", "Absolute_SWAT_Values.txt",
                              package = "SWATplusR"),
                  run_path%//%".model_run"%//%"thread"%_%i)
      } else {
        file.copy(abs_swat_val, run_path%//%".model_run"%//%"thread"%_%i)
      }
      swat_edit_config <- "2012 : SWAT Version (2009 | 2012)"
      writeLines(swat_edit_config, con = run_path%//%".model_run"%//%
                   "thread"%_%i%//%"Swat_edit.exe.config.txt")

      ## Create empty dummy folders. The Swat_edit.exe requires these to execute
      dir.create(run_path%//%".model_run"%//%"thread"%_%i%//%"Echo")
      dir.create(run_path%//%".model_run"%//%"thread"%_%i%//%"SUFI2.IN")
      dir.create(run_path%//%".model_run"%//%"thread"%_%i%//%"SUFI2.OUT")

      ## Write the batch file that will be executed to call the SWAT exe when
      ## executing SWAT in a later step
      swat_bat <- batch_temp
      swat_bat[3] <- swat_bat[3]%//%"thread"%_%i
      writeLines(swat_bat, con = run_path%//%".model_run"%//%
                   "thread"%_%i%//%"swat_run.bat")

      ## Write the batch file that is called for overwriting parameters
      swatedit_bat <- swat_bat
      swatedit_bat[4] <- "start /min /w SWAT_Edit.exe"
      writeLines(swatedit_bat, con = run_path%//%".model_run"%//%
                   "thread"%_%i%//%"swat_edit.bat")

      ## Write modified file_cio into thread folder and respective Backup folder
      writeLines(file_cio, run_path%//%".model_run"%//%"thread"%_%i)
      writeLines(file_cio, run_path%//%".model_run"%//%"thread"%_%i%//%"Backup")

      pb$tick()
    }
    pb$stop()
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

modify_file_cio <- function(project_path, start_date, end_date,
                            output_interval, years_skip,
                            rch_out_var, sub_out_var,
                            hru_out_var, hru_out_nr) {
  ## Read unmodified file.cio
  file_cio <- readLines(project_path%//%"file.cio")

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
    rch_out_var <- c(rch_out_var, rep(0, 20 - length(rch_out_var)))
    file_cio[65] <- paste0(sprintf("%4d", rch_out_var), collapse = "")
  }

  ## Overwrite custom subbasin variables if values are provided
  if(!is.null(sub_out_var)){
    if(!is.numeric(sub_out_var)) stop("'sub_out_var' must be numeric!")
    sub_out_var <- c(sub_out_var, rep(0, 15 - length(sub_out_var)))
    file_cio[67] <- paste0(sprintf("%4d", sub_out_var), collapse = "")
  }

  ## Overwrite custom HRU variables if values are provided
  if(!is.null(hru_out_var)){
    if(!is.numeric(hru_out_var)) stop("'hru_out_var' must be numeric!")
    hru_out_var <- c(hru_out_var, rep(0, 20 - length(hru_out_var)))
    file_cio[69] <- paste0(sprintf("%4d", hru_out_var), collapse = "")
  }

  ## Overwrite HRU numbers for which HRU outputs are written if values are provided
  if(!is.null(hru_out_nr)){
    if(!is.numeric(hru_out_nr)) stop("'hru_out_nr' must be numeric!")
    hru_out_nr <- c(hru_out_nr, rep(0, 20 - length(hru_out_nr)))
    file_cio[71] <- paste0(sprintf("%4d", hru_out_nr), collapse = "")
  }
  return(file_cio)
}
