#' Run SWAT+
#'
#' This function allows to run a SWAT+ project in R. Basic
#' settings for the SWAT run such as the simulation period or the time interval
#' for the outputs can be done directly. SWAT simulation outputs can be
#' defined that are returned in a 'tidy' format in R. Functionality such as model
#' parametrization, parallel execution of simulations, or incremental saving of
#' simulation runs is provided.
#'
#' @param project_path  Character string that provides the path to the SWAT project
#'   folder (i.e. TxtInOut).
#' @param output Define the output variables to extract from the SWAT model
#'   runs. See function \code{\link{define_output}} help file to see how to
#'   define simulation outputs.
#' @param parameter (optional) SWAT model parameters either provided as named
#'   vector or a tibble. The parameter changes provided with \code{parameter}
#'   are performed during the model execution accordingly. To learn how to
#'   modify parameters see the \href{https://chrisschuerz.github.io/SWATplusR/articles/SWATplusR.html}{Get started} page of \code{SWATplusR}.
#' @param start_date (optional) Start date of the SWAT simulation. Provided as
#'   character string in any ymd format (e.g. 'yyyy-mm-dd') or in Date format
#'   project are located.
#' @param end_date (optional) End date of the SWAT simulation. Provided as
#'   character string in any ymd format (e.g. 'yyyy-mm-dd') or in Date format
#'   project are located
#' @param output_interval (optional) Time interval in which the SWAT model
#'   outputs are written. Provided either as character string ("d" for daily,
#'   "m" for monthly, or "y" for yearly) or as SWAT input values (0 for monthly,
#'   1 for daily, 2 for yearly).
#' @param years_skip (optional) Integer value to define the number of simulation
#'   years that are skipped before writing SWAT model outputs.
#' @param run_index (optional) Numeric vector (e.g.\code{run_index = c(1:100,
#'   110, 115)}) to run a subset of the provided \code{parameter} sets. If NULL
#'   all provided parameter sets are used in the simulation.
#' @param run_path (optional) Character string that provides the path where the
#'   '.model_run' folder is written and the SWAT models are executed. If NULL
#'   '.model_run' is built in the project folder.
#' @param n_thread (optional) Number of threads to be used for the parallel
#'   model run. If not provided models are run on single core. The parameter is
#'   ineffective for single simulations.
#' @param save_path (optional) Character string to define the path where the
#'   model runs are saved if \code{save_file} is defined. If \code{save_path = NULL}
#'   the \code{save_file} is saved in the project_path.
#' @param save_file (optional) Character string to define the name of the folder
#'   where data bases are generated that store the simulations incrementally.
#' @param return_output (optional) Logical. Whether outputs should be returned
#'   or not. Set \code{return_out = FALSE} and provide \code{save_file} if
#'   outputs should only be saved on the hard drive and not be returned in R.
#'   '\code{Default = TRUE}
#' @param add_date (optional) Logical. If \code{add_date = TRUE} a date column
#'   is added to every simulation output table.  \code{Default = TRUE}
#' @param add_parameter (optional) Logical. If \code{add_parameter = TRUE}, the
#'   values of the parameter changes and information on the changes are saved
#'   and/or returned together with the model outputs. \code{Default = TRUE}
#' @param refresh (optional) Logical. \code{refresh = TRUE} always forces that
#'   '.model_run' is newly written when SWAT run ins started. \code{Default =
#'   TRUE}
#' @param keep_folder (optional) Logical. If \code{keep_folder = TRUE}
#'   '.model_run' is kept and not deleted after finishing model runs. In this
#'   case '.model_run' is reused in a new model run if \code{refresh = FALSE}.
#'   \code{Default = FALSE}
#' @param quiet (optional) Logical. If \code{quiet = TRUE} no messages are
#'   written.  \code{Default = FALSE}
#'
#' @section Examples:
#'   To learn the basics on how to use \code{SWATplusR} see the
#'   \href{https://chrisschuerz.github.io/SWATplusR/articles/SWATplusR.html#first-swat-model-runs}{Get started}
#'   page on the package's github page.
#' @return Returns the simulation results for the defined output variables as a
#'   tibble. If more than one parameter set was provided a list of tibbles is
#'   returned where each column is a model run and each list entry is an output
#'   variable.
#'
#' @importFrom doSNOW registerDoSNOW
#' @importFrom dplyr %>%
#' @importFrom foreach foreach %dopar%
#' @importFrom lubridate now
#' @importFrom parallel detectCores makeCluster parSapply stopCluster
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @export
run_swatplus <- function(project_path, output, parameter = NULL,
                         start_date = NULL, end_date = NULL,
                         output_interval = NULL, years_skip = NULL,
                         run_index = NULL, run_path = NULL,
                         n_thread = NULL, save_path = NULL,
                         save_file = NULL, return_output = TRUE,
                         add_parameter = TRUE, add_date = TRUE,
                         refresh = TRUE, keep_folder = FALSE,
                         quiet = FALSE) {

#-------------------------------------------------------------------------------

  # Check input parameters for additional inputs
  # Not implemented currently, might be required if soft calibration is
  # implemented
  # add_input <- as.list(match.call(expand.dots=FALSE))[["..."]]

  # Check settings before starting to set up '.model_run'
  ## General function input checks
  stopifnot(is.character(project_path))
  stopifnot(is.character(run_path)|is.null(run_path))
  stopifnot(is.numeric(n_thread)|is.null(n_thread))
  stopifnot(is.numeric(years_skip)|is.null(years_skip))
  stopifnot(is.logical(add_parameter))
  stopifnot(is.logical(add_date))
  stopifnot(is.logical(return_output))
  stopifnot(is.logical(refresh))
  stopifnot(is.logical(keep_folder))
  stopifnot(is.logical(quiet))

  ## Check if all parameter names exist in cal_parms.cal
  if(!is.null(parameter)) {
    parameter <- format_swatplus_parameter(parameter)
    check_swatplus_parameter(project_path, parameter)

    # here would also be clever to implement parameter boundary checkup keep
    # parameter boundary file in R package and write to project folder when it
    # does not exist. Otherwise read boundary file from there and do check! Jeff
    # provides some workaround in SWAT+ internally (automatic setting to
    # lower/upper boundary)
  }
  ## Check values provided with run_index and prepare run_index for simulation
  if(!is.null(run_index)){
    run_index <- check_run_index(run_index, parameter$values)
  } else {
    run_index <- 1:max(nrow(parameter$values), 1)
  }

  ## Define save_path and check if planned simulations already exist in save file
  if(!is.null(save_file)) {
    save_path <- set_save_path(project_path, save_path, save_file)
    check_saved_data(save_path, parameter, output, run_index)
  }

  ## Convert output to named list in case single unnamed output was defined
  output <- check_output(output)

  ## Check if soft_calibration was triggered by screen methods. If TRUE it
  ## forces the model setup to also write the average annula balances.
  ## Not yet implemented thus set FALSE by default
  # if("soft_calibration" %in% names(add_input)){
  #   soft_cal <- eval(add_input$soft_calibration)
  # } else {
  #   soft_cal <- FALSE
  # }
  soft_cal <- FALSE

  ## Read and modify the projects' files defining simulation period years to
  ## skip, interval, etc.
  model_setup <- setup_swatplus(project_path, parameter, output,
                                start_date, end_date,
                                output_interval, years_skip, soft_cal)

#-------------------------------------------------------------------------------
  # Build folder structure where the model will be executed
  ## Identify the required number of parallel threads to build.
  n_thread <- min(max(nrow(parameter$values),1),
                  max(n_thread,1),
                  detectCores())

  ## Set the .model_run folder as the run_path
  run_path <- ifelse(is.null(run_path), project_path, run_path)%//%".model_run"

  ## Identify operating system and find the SWAT executable in the project folder
  os <- get_os()

  ## Manage the handling of the '.model_run' folder structure.
  swat_exe <- manage_model_run(project_path, run_path, n_thread, os,
                               "plus", refresh, quiet)
  swat_rev <- check_revision(project_path, run_path, os, swat_exe)
  cat("swat revision is ",swat_rev,"\n")
  output <- translate_outfile_names(output, model_setup$output_interval, swat_rev)
#-------------------------------------------------------------------------------
  # Write files
  ## Write model setup: Files that define the time range etc. of the SWAT
  ## simulation
  write_swatplus_setup(run_path, model_setup)

  ## Initialize the save_file if defined
  if(!is.null(save_file)) {
    initialize_save_file(save_path, parameter, model_setup)
  }
  #-------------------------------------------------------------------------------
  # Initiate foreach loop to run SWAT models
  ## make and register cluster, create table that links the parallel worker
  ## with the created parallel thread folders in '.model_run'
  cl <- makeCluster(n_thread)
  worker <- tibble(worker_id = parSapply(cl, 1:n_thread,
                                         function(x) paste(Sys.info()[['nodename']],
                                                           Sys.getpid(), sep = "-")),
                   thread_id = dir(run_path) %>% .[grepl("thread_",.)])

  registerDoSNOW(cl)
  #-------------------------------------------------------------------------------
  # Start parallel SWAT model execution with foreach

  ## If not quiet a function for displaying the simulation progress is generated
  ## and provided to foreach via the SNOW options
  n_run <- length(run_index)
  if(!quiet) {
    cat("Performing", n_run, "simulation"%&%plural(n_run),"on", n_thread,
        "core"%&%plural(n_thread)%&%":", "\n")
    t0 <- now()
    progress <- function(n){
      display_progress(n, n_run, t0, "Simulation")
    }
    opts <- list(progress = progress)
  } else {
    opts <- list()
  }

  sim_result <- foreach(i_run = 1:n_run,
    .packages = c("dplyr", "lubridate"), .options.snow = opts) %dopar% {
    # for(i_run in 1:max(nrow(parameter), 1)) {
    ## Identify worker of the parallel process and link it with respective thread
    worker_id <- paste(Sys.info()[['nodename']], Sys.getpid(), sep = "-")
    thread_id <- worker[worker$worker_id == worker_id, 2][[1]]
    thread_path <- run_path%//%thread_id
    # thread_path <- run_path%//%"thread_1"

        ## Modify model parameters if parameter set was provided and write
    ## calibration file. If no parameters provided write empty calibration file
    if(is.null(parameter)) {
      if(file.exists(thread_path%//%"calibration.cal")) {
        file.remove(thread_path%//%"calibration.cal")
      }
    } else {
      write_calibration(thread_path, parameter, model_setup$calibration.cal,
                        run_index, i_run)
    }

    ## Execute the SWAT exe file located in the thread folder
    if(os == "win") {
      run_batch <- thread_path%//%"swat_run.bat"
    } else if (os == "unix") {
      run_batch <- paste("cd", "cd"%&&%thread_path, "./"%&%swat_exe, sep = "; ")
    }
    #browser()
    run_msg <- system(file.path(run_batch), intern = T)
    #writeLines(run_msg,"batchrunlog.txt")
    ## Read defined model outputs
    model_output <- read_swatplus_output(output, thread_path, swat_rev) %>%
      extract_output(output, .)

    if(!is.null(save_path)) {
      save_run(save_path, model_output, parameter, run_index, i_run, thread_id)
    }
    if(return_output) {
      return(model_output)
    }
  }

  ## Stop cluster after parallel run
  stopCluster(cl)

  ## Show total runs and elapsed time in console if not quiet
  if(!quiet) {
    finish_progress(n_run, t0, "simulation")
    ## Delete the time stamp t0 created for the progress estimation
    rm(t0)
  }

  ##Tidy up results if return_output is TRUE
  if(return_output) {
    ## Create date vector from the information in model_setup
    date <- get_date_vector(model_setup)
    ## Tidy up the simulation results and arrange them in clean tibbles before
    ## returning them
    sim_result <- tidy_results(sim_result, parameter, date, add_parameter,
                               add_date, run_index)

  }
  ## Delete the parallel threads if keep_folder is not TRUE
  if(!keep_folder)unlink(run_path, recursive = TRUE)

  ## ...and return simulation results if return_output is TRUE
  if(return_output) return(sim_result)
}
