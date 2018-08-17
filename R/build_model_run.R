#' Generate folder structure for parallel SWAT execution
#'
#' @param project_path Project path where the parallel folder structure should
#' be created.
#' @param txtIO_path TxtInOut path where the text in out files of the SWAT
#' project are located
#' @param n_thread Number of parallel threads that will be created. This
#' number must be in accordance to the number of cores of the PC
#' @param swat_version SWAT version, either 2012 or 2009
#'
#' @importFrom parallel detectCores
#' @import dplyr
#' @importFrom pasta %//% %_%
#' @keywords internal



build_model_run <- function(project_path, n_thread){
  # Identify operating system and find the SWAT executable in the project folder
  os <- get_os()
  if(os == "win") {
    exe_file <- list.files(project_path) %>%
      .[grepl(".exe$",.)]
  } else if(os == "unix") {
    exe_file <- system("find"%&&%project_path%&&%"-executable -type f",
                       intern = TRUE) %>%
      basename(.)
  }

  # Make shure that there is exactly one executable in the SWAT project folder
  if(length(exe_file) == 0) stop("No SWAT executable found in project folder!")
  if(length(exe_file) > 1) stop("Project folter contains more than one executable!")

  # Batch file template
  batch_temp <- list(win = c("@echo off",
                             substr(project_path, 1, 2),
                             paste("cd", project_path, sep = " "),
                             exe_file,
                             "if %errorlevel% == 0 exit 0",
                             "echo."),
                     unix = NULL) #Required batch for running SWAT in Linux coming soon

  # Create folder structure to execute SWAT
  if(os == "win") {
    print("Building folder '.model_run' for the SWAT model execution:")
    swat_files <- dir(project_path, full.names = TRUE)
    dir.create(project_path%//%".model_run")
    pb <- progress_estimated(n_thread)
    for (i in 1:n_thread){
      pb$begin()$print()
      dir.create(project_path%//%".model_run"%//%"thread"%_%i)
      file.copy(swat_files, project_path%//%".model_run"%//%"thread"%_%i)
      dir.create(project_path%//%".model_run"%//%"thread"%_%i%//%"Backup")
      file.copy(swat_files,
                project_path%//%".model_run"%//%"thread"%_%i%//%"Backup")
      file.copy(system.file("extdata", "Swat_Edit.exe",
                            package = "SWATplusR"),
                project_path%//%".model_run"%//%"thread"%_%i)
      file.copy(system.file("extdata", "SUFI2_execute.exe",
                            package = "SWATpasteR"),
                project_path%//%".model_run"%//%"thread"%_%i)
      file.copy(system.file("extdata", "Absolute_SWAT_Values.txt",
                            package = "SWATpasteR"),
                project_path%//%".model_run"%//%"thread"%_%i)
      swat_edit_config <- "2012 : SWAT Version (2009 | 2012)"
      writeLines(swat_edit_config, con = project_path%//%".model_run"%//%
                   "thread"%_%i%//%"Swat_edit.exe.config.txt")
      dir.create(project_path%//%".model_run"%//%"thread"%_%i%//%"Echo")
      dir.create(project_path%//%".model_run"%//%"thread"%_%i%//%"SUFI2.IN")
      dir.create(project_path%//%".model_run"%//%"thread"%_%i%//%"SUFI2.OUT")
      swat_bat <- batch_temp
      swat_bat[3] <- swat_bat[3]%//%"thread"%_%i
      writeLines(swat_bat, con = project_path%//%".model_run"%//%
                   "thread"%_%i%//%"swat_run.bat")
      swatedit_bat <- swat_bat
      swatedit_bat[4] <- "start /min /w SWAT_Edit.exe"
      writeLines(swatedit_bat, con = project_path%//%".model_run"%//%
                   "thread"%_%i%//%"swat_edit.bat")
      pb$tick()
    }
    pb$stop()
  }
}

#' Identify the OS (provided by Gabor Csardi)
#' @keywords internal
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
