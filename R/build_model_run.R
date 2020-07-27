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

    # Batch file template required to run swat on Windows
    batch_temp <- c("@echo off",
                    "cd"%&&%str_sub(run_path, 1, 2),
                    "cd"%&&%run_path,
                    swat_exe,
                    "if %errorlevel% == 0 exit 0",
                    "echo.")

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
    if(os == "win") {
    ## Write the batch file that will be executed to call the SWAT exe when
    ## executing SWAT in a later step
    swat_bat <- batch_temp
    swat_bat[3] <- swat_bat[3]%//%"thread"%_%i
    writeLines(swat_bat, con = run_path%//%"thread"%_%i%//%"swat_run.bat")
    }
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

#' Check the Revision of used SWAT+ executable
#'
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#' @param run_path Path where the '.model_run' folder is built. If NULL the
#'   executable model is built in the 'project_path'
#' @param os Character string that gives the operationg system
#' @param swat_exe Character string that gives the name of the SWAT executable.
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_sub
#' @keywords internal
#'
check_revision <- function(project_path, run_path, os, swat_exe) {
  dir.create(run_path%//%"tmp")

  swat_files <- dir(project_path, full.names = TRUE) %>%
    .[!grepl(".txt$|.csv$|.db$",.)]

  file.copy(swat_files, run_path%//%"tmp")

  if(os == "win") {
    # Batch file template required to run swat on Windows
    batch_temp <- c("@echo off",
                    "cd"%&&%str_sub(run_path, 1, 2),
                    "cd"%&&%run_path%//%"tmp",
                    swat_exe,
                    "if %errorlevel% == 0 exit 0",
                    "echo.")

    run_batch <- run_path%//%"tmp"%//%"swat_run.bat"
    writeLines(batch_temp, con = run_batch)

  } else if(os == "unix") {
    run_batch <- paste("cd", "cd"%&&%run_path%//%"tmp", "./"%&%swat_exe, sep = "; ")
  }
  #browser()
  tmp_msg <- suppressWarnings(system2(file.path(run_batch), timeout = 2, stdout = T)) 
  writeLines(tmp_msg,"revisonlogfile.txt")
  tmp_msg <- tmp_msg %>%
    .[grepl("Revision", .)] %>%
    gsub("Revision", "", .) %>%
    trimws(.) %>%
    as.numeric(.)
  
  #Sys.sleep(1)

  unlink(run_path%//%"tmp",recursive = TRUE, force = TRUE)

  return(tmp_msg)
}
