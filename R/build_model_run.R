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

build_model_run <- function(project_path, run_path, n_thread, quiet){
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
    cat("Building", n_thread, "thread"%&%plural(n_thread),
        "in","'"%&%run_path%&%"':", "\n")
  }

  # Create folder structure to execute SWAT
  if(os == "win") {
    swat_files <- dir(project_path, full.names = TRUE)
    ## To save storage do not copy allready existing output files
    ## Extend for SWAT+ output files after discussion about names with Jeff
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
