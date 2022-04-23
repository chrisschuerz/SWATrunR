#' Run a SWAT2012 project
#'
#' This function allows to run a SWAT2012 project in R.
#' Basic settings for the SWAT run such as the simulation period or the time
#' interval for the outputs can be done directly. SWAT simulation outputs can be
#' defined that are returned in a 'tidy' format in R. Functionality such as model
#' parametrization, parallel execution of simulations, or incremental saving of
#' simulation runs is provided.
#'
#' @param project_path Character string that provides the path to the SWAT project
#'   folder (i.e. TxtInOut).
#' @param output Define the output variables to extract from the SWAT model
#'   runs. See function \code{\link{define_output}} help file to see how to
#'   define simulation outputs.
#' @param parameter (optional) SWAT model parameters either provided as named
#'   vector or a tibble. The parameter changes provided with \code{parameter}
#'   are performed during the model execution accordingly. To learn how to
#'   modify parameters see the \href{https://chrisschuerz.github.io/SWATplusR/articles/SWATplusR.html}{Get started} page of \code{SWATplusR}.
#' @param start_date (optional) Start date of the SWAT simulation. Provided as
#'   character string in a ymd format (e.g. 'yyyy-mm-dd') or in Date format.
#' @param end_date (optional) End date of the SWAT simulation. Provided as
#'   character string in a ymd format (e.g. 'yyyy-mm-dd') or in Date format.
#' @param output_interval (optional) Time interval in which the SWAT model
#'   outputs are written. Provided either as character string ("d" for daily,
#'   "m" for monthly, or "y" for yearly) or as SWAT input values (0 for monthly,
#'   1 for daily, 2 for yearly).
#' @param years_skip (optional) Integer value to define the number of simulation
#'   years that are skipped before writing SWAT model outputs.
#' @param rch_out_var (optional) Numeric vector of maximum \code{length = 20} for
#'   customized output of reach variables. For output codes see the
#'   \href{https://swat.tamu.edu/media/69308/ch03_input_cio.pdf}{SWAT I/O
#'   Documentation} p.77ff.
#' @param sub_out_var (optional) Numeric vector of maximum \code{length = 15} for
#'   customized output of subbasin variables.For output codes see the
#'   \href{https://swat.tamu.edu/media/69308/ch03_input_cio.pdf}{SWAT I/O
#'   Documentation} p.78ff.
#' @param hru_out_var (optional) Numeric vector of maximum \code{length = 20} for
#'   customized output of HRU variables.For output codes see the
#'   \href{https://swat.tamu.edu/media/69308/ch03_input_cio.pdf}{SWAT I/O
#'   Documentation} p.79ff.
#' @param hru_out_nr (optional) Numeric vector of maximum \code{length = 20} for
#'   providing the HRU numbers for which the HRU variables are written. Optional
#'   if \code{hru_out_nr = 'all'}, HRU variables are written for all HRU
#'   (caution, very large output files possible!)
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
#'   the folder \code{save_file} is saved in the project_path.
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
#' @importFrom processx run
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @export

run_swat2012 <- function(project_path, output, parameter = NULL,
                         start_date = NULL, end_date = NULL,
                         output_interval = NULL, years_skip = NULL,
                         rch_out_var = NULL, sub_out_var = NULL,
                         hru_out_var = NULL, hru_out_nr = NULL,
                         run_index = NULL, run_path = NULL,
                         n_thread = NULL, save_path = NULL,
                         save_file = NULL, return_output = TRUE,
                         add_parameter = TRUE, add_date = TRUE,
                         refresh = TRUE, keep_folder = FALSE, quiet = FALSE) {

#-------------------------------------------------------------------------------
  # Check settings before starting to set up '.model_run'
  ## General function input checks
  stopifnot(is.character(project_path))
  stopifnot(is.character(run_path)|is.null(run_path))
  stopifnot(is.numeric(n_thread)|is.null(n_thread))
  stopifnot(is.logical(add_parameter))
  stopifnot(is.logical(add_date))
  stopifnot(is.logical(return_output))
  stopifnot(is.logical(refresh))
  stopifnot(is.logical(keep_folder))
  stopifnot(is.logical(quiet))

  ## Check if all parameter names exist in the Absolute_SWAT_Value.txt
  if(!is.null(parameter)) {
    parameter <- format_swat2012_parameter(parameter, '2012')
  }

  ## Check values provided with run_index and prepare run_index for simulation
  if(!is.null(run_index)){
    run_index <- check_run_index(run_index, parameter$values)
  } else {
    run_index <- 1:max(nrow(parameter$values), 1)
  }

  ## Convert output to named list in case single unnamed output was defined
  output <- check_output(output, "2012")

  ## Read the meta information on the parameters and the required parameter files
  if(!is.null(parameter)) {
    file_meta <- read_file_meta(project_path, parameter$definition)
    swat_parameter <- read_swat2012_files(project_path,file_meta)
  # here would be clever to implement parameter boundary checkup
  # keep parameter boundary file in R package and write to project folder when
  # it does not exist. Otherwise read boundary file from there and do check!
  }


  ## Read and modify the projects' file.cio, internal variable checks done.
  model_setup <- setup_swat2012(project_path, output,
                                start_date, end_date,
                                output_interval, years_skip,
                                rch_out_var, sub_out_var,
                                hru_out_var, hru_out_nr)

  ## Define save_path and check if planned simulations already exist in save file
  if(!is.null(save_file)) {
    save_path <- set_save_path(project_path, save_path, save_file)
    check_saved_data(save_path, parameter, output, run_index, model_setup)
  }

  # Check if weather inputs accord with start and end date
  check_dates(project_path, model_setup)

#-------------------------------------------------------------------------------
  # Build folder structure where the model will be executed
  ## Identify the required number of parallel threads to build.
  n_thread <- min(max(nrow(parameter$values),1),
                  max(n_thread,1),
                  max(length(run_index),1),
                  detectCores())

  ## Set the .model_run folder as the run_path
  run_path <- ifelse(is.null(run_path), project_path, run_path)%//%".model_run"

  ## Identify operating system and find the SWAT executable in the project folder
  os <- get_os()

  ## Manage the handling of the '.model_run' folder structure.
  swat_exe <- manage_model_run(project_path, run_path, n_thread, os,
                               "2012", refresh, quiet)
#-------------------------------------------------------------------------------
  # Write files
  ## Write file.cio
  write_file_cio(run_path, model_setup$file.cio)

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
    cat("Performing", n_run, ifelse(n_run == 1, "simulation", "simulations"),
        "on", n_thread, "cores:", "\n")
    t0 <- now()
    progress <- function(n){
      display_progress(n, n_run, t0, "Simulation")
    }
    opts <- list(progress = progress)
  } else {
    opts <- list()
  }

 sim_result <- foreach(i_run = 1:n_run,
                       .packages = c("dplyr", "lubridate", "stringr", "processx"),
                       .options.snow = opts) %dopar% {
   # for(i_run in 1:max(nrow(parameter$values), 1)) {
   # Identify worker of the parallel process and link it with respective thread
    worker_id <- paste(Sys.info()[['nodename']], Sys.getpid(), sep = "-")
    thread_id <- worker[worker$worker_id == worker_id, 2][[1]]
    thread_path <- run_path%//%thread_id
    # thread_path <- run_path%//%"thread_1"

    ## Modify model parameters if parameter set was provided
    if(!is.null(parameter)) {
      thread_parameter <- swat_parameter
      thread_parameter <- modify_parameter(parameter, thread_parameter,
                                           file_meta, run_index, i_run)
      write_parameter(file_meta, thread_parameter, thread_path)
    }

    ## Execute the SWAT exe file located in the thread folder
    msg <- run(run_os(swat_exe, os), wd = thread_path, error_on_status = FALSE)

    if(nchar(msg$stderr) == 0) {
      ## Read defined model outputs
      model_output <- read_swat2012_output(output, thread_path) %>%
        extract_output(output, .)

      if(!is.null(save_path)) {
        save_run(save_path, model_output, parameter, run_index, i_run, thread_id)
      }
    } else {
      err_msg <- str_split(msg$stderr, '\r\n|\r|\n', simplify = TRUE)
      out_msg <- str_split(msg$stdout, '\r\n|\r|\n', simplify = TRUE) %>%
        .[max(1, length(.) - 10):length(.)]
      err_msg <- c('Last output:', out_msg, 'Error:', err_msg)
      model_output <- err_msg
      if(!is.null(save_path)) {
        save_error_log(save_path, model_output, parameter, run_index, i_run)
      }
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

  ## Delete the parallel threads if keep_folder is not TRUE
  if(!keep_folder) unlink(run_path, recursive = TRUE)

  ##Tidy up and return simulation results if return_output is TRUE
  if(return_output) {
    ## Create date vector from the information in model_setup
    date <- get_date_vector_2012(model_setup)
    ## Tidy up the simulation results and arrange them in clean tibbles before
    ## returning them
    sim_result <- tidy_results(sim_result, parameter, date, add_parameter,
                               add_date, run_index)
    if("error_report" %in% names(sim_result)) {
      warning("Some simulations runs failed! Check '.$error_report' in your",
              " simulation results for further information.")
    }
    return(sim_result)
  }
}
