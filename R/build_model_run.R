#' Generate folder structure for parallel SWAT execution
#'
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#' @param run_path Path where the '.model_run' folder is built. If NULL the
#'   executable model is built in the 'project_path'
#' @param n_thread Number of parallel threads that will be created. This number
#'   must be in accordance to the number of cores of the PC
#' @param os String that indicates the operating system on the current machine
#' @param swat_vers Character string that defines the SWAT version. Either
#'   "2012" or "plus".
#' @param refresh Logical. Defines if refreshing exisiting .model_run folder
#'   structure should be forced.
#' @param quiet Logical. Defines if messages should be written or function
#'   should be executed quietly.
#'
#' @importFrom dplyr %>%
#' @keywords internal
#'

manage_model_run <- function(project_path, run_path, n_thread, os,
                             swat_vers, refresh, quiet) {
  ## Case .model_run exists already and no forced refresh considered
  if(dir.exists(run_path) & !refresh) {
    ## Check how many parallel threads are available
    n_thread_avail <- dir(run_path) %>%
      gsub('[0-9]+', '', .) %>%
      grepl("thread_",.) %>%
      sum()
    ## The existing folder strucuture is used when more parallel folders are
    ## available than parallel threads are needed
    if(n_thread_avail >= n_thread) {
      if(!quiet) {
        message("Model will be executed in existing '.model_run' folder structure"%&%
                  "\nMake sure '.model_run' is up to date with the project folder!")
      }
      ## If the number of available parallel folders is not sufficient
      ## a new setup of the folder structures is forced
    } else {
      unlink(run_path, recursive = TRUE)
      if(!quiet) {
        message("The number of existing threads is lower than the required number."%&%
                  "\nParallel folder structure will be created from scratch!\n\n")
      }
      build_model_run(project_path, run_path, n_thread, os, swat_vers, quiet)
    }
    ## Build the parallel folder structure if it does not exist or if a
    ## forced refresh was set with refresh = TRUE
  } else {
    unlink(run_path, recursive = TRUE)
    build_model_run(project_path, run_path, n_thread, os, swat_vers, quiet)
  }
}

#' Generate folder structure for parallel SWAT execution
#'
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#' @param run_path Path where the '.model_run' folder is built. If NULL the
#'   executable model is built in the 'project_path'
#' @param n_thread Number of parallel threads that will be created. This number
#'   must be in accordance to the number of cores of the PC
#' @param swat_vers Character string that defines the SWAT version. Either
#'   "2012" or "plus".
#' @param quiet Logical. Defines if messages should be written or function
#'   should be executed quietly.
#'
#' @importFrom parallel detectCores
#' @importFrom dplyr %>%
#' @importFrom lubridate now
#' @importFrom stringr str_sub
#' @keywords internal
#'

build_model_run <- function(project_path, run_path, n_thread, os, swat_vers, quiet){

  if(os == "win") {
    swat_exe <- list.files(project_path) %>%
      .[grepl(".exe$",.)]

  } else if(os == "unix") {
    swat_exe <- system("find"%&&%project_path%&&%"-executable -type f",
                        intern = T) %>%
      basename(.)
  }

  # Make sure that there is exactly one executable in the SWAT project folder
  if(length(swat_exe) == 0) stop("No SWAT executable found in the project folder!")
  if(length(swat_exe) > 1) stop("Project folder contains more than one executable!")

  if(!quiet) {
    cat("Building", n_thread, "thread"%&%plural(n_thread),
        "in","'"%&%run_path%&%"':", "\n")
  }

  # Create folder structure to execute SWAT

  swat_files <- dir(project_path, full.names = TRUE)
  ## To save storage do not copy allready existing output files
  ## Extend for SWAT+ output files after discussion about names with Jeff
  if(swat_vers == "2012") {
    exclude <- "output.hru|output.pst|output.rch|output.rsv|output.sed|output.std|output.sub"
  } else {
    exclude <- ".txt$|.csv$|.db$"
  }
  swat_files <- swat_files[!grepl(exclude,swat_files)]

  dir.create(run_path, recursive = TRUE)
  t0 <- now()
  for (i in 1:n_thread){
    ## Copy all files from the project folder to the respective thread
    dir.create(run_path%//%"thread"%_%i)
    file.copy(swat_files, run_path%//%"thread"%_%i)
    if(!quiet) {
      display_progress(i, n_thread, t0, "Thread")
    }
  }
  if(!quiet){
    finish_progress(n_thread, t0, "thread")
  }
  return(swat_exe)
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
    "unix"
  } else {
    stop("Unknown OS")
  }
}

#' Add './' to run the exe on unix systems
#' @param exe Text string that defines the name of the executable file
#' @param os Text string that defines the operating system
#' @keywords internal
#'
run_os <- function(exe, os) {
  if(os == 'unix') exe <- '.'%//%exe
  return(exe)
}

#' Check the Revision of used SWAT+ executable
#'
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#' @param run_path Path where the '.model_run' folder is built. If NULL the
#'   executable model is built in the 'project_path'
#' @param os Character string that gives the operationg system
#' @param swat_exe Character string that gives the name of the SWAT executable.
#'
#' @importFrom dplyr %>%
#' @importFrom processx run
#' @importFrom stringr str_split str_remove_all
#' @keywords internal
#'
check_revision <- function(project_path, run_path, os, swat_exe) {
  tmp_path <- run_path%//%"tmp"

  dir.create(tmp_path, recursive = T)
  file.copy(project_path%//%swat_exe, tmp_path)

  tmp_msg <- run(run_os(swat_exe, os), wd = tmp_path, error_on_status = FALSE) %>%
    .$stdout %>%
    str_split(., '\r\n|\n', simplify = T) %>%
    .[2] %>%
    str_remove_all(., '[:alpha:]') %>%
    str_extract(., '[:digit:]{0,}\\.?[:digit:]') %>%
    as.numeric()

  unlink(tmp_path, recursive = TRUE, force = TRUE)

  return(tmp_msg)
}

#' Check if input start and end dates are given in the weather inputs
#'
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#' @param model_setup list of model setup features
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate year
#' @importFrom readr read_lines
#' @importFrom stringr str_sub
#' @keywords internal
#'
check_dates <- function(project_path, model_setup) {
  pcp_files <- list.files(project_path, pattern = ".pcp")
  pcp <- read_lines(project_path%//%pcp_files[1], lazy = FALSE)
  start_year <- suppressWarnings(as.numeric(str_sub(pcp[4:6],1,4))) %>%
    median(., na.rm = TRUE)
  end_year <- suppressWarnings(as.numeric(str_sub(pcp[(length(pcp)-20):length(pcp)],1,4))) %>%
    max(., na.rm = TRUE)

  start_sim <- year(model_setup$start_date)
  end_sim   <- year(model_setup$end_date)

  if(start_sim < start_year) {
    stop("Defined 'start_date' is earlier than available weather input time series.")
  }

  if(end_sim > end_year) {
    stop("Defined 'end_date' is later than available weather input time series.")
  }
}
