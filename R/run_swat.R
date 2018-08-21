#' Run a SWAT project and return ouputs in R
#'
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#' @param output Define the output variables to extract from the SWAT model
#'   runs. See function \code{\link{define_output}} help file to see how to
#'   define an output.
#' @param parameter (optional) SWAT model parameters either provided as named
#'   vector or data.frame. If \code{parameter} is provided respective parameters
#'   are modified accordingly.
#' @param start_date (ptional) Start date of the SWAT simulation. Provided as
#'   character string in any ymd format (e.g. 'yyyy-mm-dd') or in Date format
#'   project are located
#' @param end_date (optional) End date of the SWAT simulation. Provided as
#'   character string in any ymd format (e.g. 'yyyy-mm-dd') or in Date format
#'   project are located
#' @param output_interval (optional) Time interval in which the SWAT model
#'   outputs are written. Provided either as character string ("d" for daily,
#'   "m" for monthly, or "y" for yearly) or as SWAT input values (0 for monthly,
#'   1 for daily, 2 for yearly).
#' @param years_skip (optional) Integer value that provides the numbe of years
#'   to be skipped during writing the SWAT model outputs
#' @param rch_out_var (optional) Numeric vector of maximum length = 20 for
#'   customized output of reach variables.For output codes see
#'   \href{https://swat.tamu.edu/media/69308/ch03_input_cio.pdf}{SWAT I/O
#'   Documentation} p.77ff.
#' @param sub_out_var (optional) Numeric vector of maximum length = 15 for
#'   customized output of subbasin variables.For output codes see
#'   \href{https://swat.tamu.edu/media/69308/ch03_input_cio.pdf}{SWAT I/O
#'   Documentation} p.78ff.
#' @param hru_out_var (optional) Numeric vector of maximum length = 20 for
#'   customized output of HRU variables.For output codes see
#'   \href{https://swat.tamu.edu/media/69308/ch03_input_cio.pdf}{SWAT I/O
#'   Documentation} p.79ff.
#' @param hru_out_nr (optional) Numeric vector of maximum length = 20 for
#'   providing the HRU numbers for which the HRU variables are written. Optional
#'   if hru_out_nr = 'all', HRU variables are written for all HRU (caution, very
#'   large output files possible!)
#' @param abs_swat_val (optional) \code{run_swat2012} uses an internal
#'   'Absolute_SWAT_Values.txt' file required for overwriting parameters. With
#'   this parameter the path to a custom 'Absolute_SWAT_Values' file can be
#'   provided in case it is necessary.
#' @param run_index (optional) Numeric vector (e.g.\code{run_index = c(1:100,
#'   110, 115)}) to run a subset of the provided \code{parameter} sets. If NULL
#'   all provided parameter sets are used.
#' @param run_path (optional) Character string that provides the path where the
#'   '.model_run' folder is written and the SWAT models are executed. If NULL
#'   '.model_run' is built in the project folder.
#' @param n_thread (optional) Number of threads to be used for the parallel
#'   model run. If not provided models are run on single core
#' @param save_file (optional) Character string that provides file path to which
#'   model results are saved after all model runs are completed. (Load
#'   \code{save_file} with \code{file <- readRDS(save_file)}).
#' @param save_incr (optional) Logical. If \code{save_incr = TRUE} model runs
#'   are saved incrementally and can be restored with \code{\link{recover_run}}.
#' @param save_parameter (optional) Logical. If \code{save_parameter = TRUE}
#'   used parameter sets are saved and/or returned together with the model
#'   outputs.
#' @param return_out (optional) Logical. Whether outputs should be returned or
#'   not. Set \code{return_out = FALSE} and provide \code{save_file} if outputs
#'   should only be saved on hard drive.
#' @param refresh (optional) Logical. \code{refresh = TRUE} always forces that
#'   '.model_run' is newly written when SWAT run ins started.
#' @param keep_folder (optional) Logical. If \code{keep_folder = TRUE}
#'   '.model_run' is kept and not deleted after finishing model runs. In this
#'   case '.model_run' is reused in a new model run if \code{refresh = FALSE}.
#' @param quiet (optional) Logical. If \code{quiet = TRUE} no messages are
#'   written.
#'
#' @return Returns the simulation results for the defined output variables as a
#'   named list. If more than one parameter set was provided the list contains a
#'   tibble where each column is a model run.
#'
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom parallel detectCores makeCluster parSapply stopCluster
#' @importFrom tibble tibble
#' @export

run_swat2012 <- function(project_path, output, parameter = NULL,
                         start_date = NULL, end_date = NULL,
                         output_interval = NULL, years_skip = NULL,
                         rch_out_var = NULL, sub_out_var = NULL,
                         hru_out_var = NULL, hru_out_nr = NULL,
                         abs_swat_val = NULL, run_index = NULL, run_path = NULL,
                         n_thread = NULL, save_file = NULL, save_incr = FALSE,
                         save_parameter = TRUE, return_out = TRUE,
                         refresh = FALSE, keep_folder = FALSE, quiet = FALSE) {

#-------------------------------------------------------------------------------
  # Check settings before starting to set up '.model_run'
  ## Check if all parameter names exist in the Absolute_SWAT_Value.txt
  if(!is.null(parameter)) check_parameter(parameter, abs_swat_val)

  ## Check values provided with run_index and prepare run_index for simulation
  if(!is.null(run_index)){
    run_index <- check_run_index(run_index, parameter)
  } else {
    run_index <- 1:max(nrow(parameter), 1)
  }

  ## Check if save file already exists
  if(!is.null(save_file)) {
    if(file.exists(save_file)) stop("'save_file' allready exists in provided path!")
  }

  ## General function input checks
  stopifnot(is.character(project_path))
  stopifnot(is.character(run_path)|is.null(run_path))
  stopifnot(is.numeric(n_thread)|is.null(n_thread))
  stopifnot(is.logical(save_incr))
  stopifnot(is.logical(save_parameter))
  stopifnot(is.logical(return_out))
  stopifnot(is.logical(refresh))
  stopifnot(is.logical(keep_folder))
  stopifnot(is.logical(quiet))

  ## Read and modify the projects' file.cio, internal variable checks done.
  file_cio <- modify_file_cio(project_path, start_date, end_date,
                              output_interval, years_skip,
                              rch_out_var, sub_out_var,
                              hru_out_var, hru_out_nr)

#-------------------------------------------------------------------------------
  # Build folder structure where the model will be executed
  ## Identify the required number of parallel threads to build.
  n_thread <- min(max(nrow(parameter),1),
                  max(n_thread,1),
                  detectCores())

  ## Set the .model_run folder as the run_path
  if(is.null(run_path)){
    run_path <- project_path%//%".model_run"
  } else {
    run_path <- run_path%//%".model_run"
  }

  ## Case .model_run exists already and no forced refresh considered
  if(dir.exists(run_path) & !refresh) {
    ## Check how many parallel threads are available
    n_thread_avail <- dir(run_path) %>%
      substr(.,(nchar(.) - 7), nchar(.)) %>%
      grepl("thread_",.) %>%
      sum()
    ## The existing folder strucuture is used when more parallel folders are
    ## available than parallel threads are needed
    if(n_thread_avail >= n_thread) {
      if(!quiet) {
        message("Model will be executed in existing '.model_run' folder structure"%&%
                "\nMake shure '.model_run' is up to date with the project folder!")
      }
    ## If the number of available parallel folders is not sufficient
    ## a new setup of the folder structures is forced
    } else {
      unlink(run_path, recursive = TRUE)
      if(!quiet) {
        message("The number of existing threads is lower than the required number."%&%
                "\nParallel folder structure will be created from scratch!\n\n")
      }
      build_model_run(project_path, run_path, n_thread, abs_swat_val, quiet)
    }
    ## Build the parallel folder structure if it does not exist or if a
    ## forced refresh was set with refresh = TRUE
    } else {
      build_model_run(project_path, run_path, n_thread, abs_swat_val, quiet)
    }

  ## Write file.cio
  write_file_cio(run_path, file_cio)

#-------------------------------------------------------------------------------
  # Initiate foreach loop to run SWAT models
  ## make and register cluster, create table that links the parallel worker
  ## with the created parallel thread folders in '.model_run'
  cl <- makeCluster(n_thread)
  worker <- tibble(worker_id = parSapply(cl, 1:n_thread,
                   function(x) paste(Sys.info()[['nodename']],
                                     Sys.getpid(), sep = "-")),
                   thread_id = dir(run_path) %>% .[grepl("thread_",.)])

  registerDoParallel(cl)
#-------------------------------------------------------------------------------
  # Start parallel SWAT model execution with foreach
  sim <- foreach(i_run = 1:max(nrow(parameter), 1),
                 .packages = c("dplyr", "pasta")) %do% {

    ## Identify worker of the parallel process and link it with respective thread
    worker_id <- paste(Sys.info()[['nodename']], Sys.getpid(), sep = "-")
    thread_id <- worker[worker$worker_id == worker_id, 2][[1]]
    thread_path <- run_path%//%thread_id


    ## Modify model parameters if parameter set was provided
    if(!is.null(parameter)) {
      write_model_in(parameter, thread_path, i_run)
      system(thread_path%//%"swat_edit.bat")
    }

    system(thread_path%//%"swat_run.bat")
    out_structure <- lapply(out_names, get_outstruct, thread_dir) %>%
      set_names(out_names)

    output_file <- lapply(out_names, read_output, out_structure, thread_dir) %>%
      set_names(out_names)

    sim_lst <- apply(output, 1, extract_var, output_file, ind) %>%
      set_names(output$label)

  }

  ## Stop cluster after parallel run
  stopCluster(cl)

  ## Delete the parallel threads if keep_folder is not TRUE
  if(!keep_folder)unlink(run_path, recursive = TRUE)

  return(sim)

}
