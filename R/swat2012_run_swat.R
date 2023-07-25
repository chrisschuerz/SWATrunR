#' Run simulations for a SWAT2012 project
#'
#' @description
#' `run_swat2012()` performs simulations for a SWAT2012 model setup that is located
#' in the folder `project_path` and returns simulated output variables which are
#' defined with `output` in a tidy format. Optionally, a set of parameter changes
#' can be defined with `parameter` which are then applied in the simulation runs.
#'
#' Several settings such as the definition of the simulation period, incremental
#' saving of simulation results into SQLite data bases, or the use of multiple
#' cores for the simulation runs can be defined.
#'
#' @param project_path  Path to the SWAT project folder on the hard drive
#'   (i.e. txtinout folder).
#'
#' @param output Output variables to extract and return for each SWAT model run.
#'
#'   Each output variable which should be returned is defined with \code{\link{define_output}}.
#'   If multiple output variables should be returned for a model run, the output
#'   definitions must be concatenated in a named list. See the examples in the
#'   section below and on the help page of \code{\link{define_output}}.
#'
#' @param parameter (optional) Model parameter changes to be implemented in the
#'   simulation runs.
#'
#'   Parameter changes can be defined by providing a named vector with the
#'   changes for a single parameter combination or a tibble where each row is
#'   a parameter combinantion which should be used in a model simulation.
#'
#'   For examples of the implementation of parameter changes in model simulations
#'   see the
#'   \href{https://chrisschuerz.github.io/SWATplusR/articles/SWATrunR.html}{`SWATrunR`}
#'   github page and the examples in corresponding section below.
#'
#' @param start_date,end_date (optional) Start and end dates of the SWAT simulation.
#'
#'   Start and end dates have to be defined together. Default values are
#'   `NULL`. In this case the start and end dates which are defined in file
#'   'time.sim' of the SWAT+ project are used in the simulation runs.
#'
#'   The dates can be provided as character strings in any ymd format
#'   (e.g. 'yyyy-mm-dd'), as numeric values (yyyymmdd) or in a `Date`
#'   format.
#'
#' @param output_interval (optional) Time interval in which the SWAT model
#'   outputs are written.
#'
#'   If `NULL` (default) the value which is defined in the file.cio is used.
#'   `output_interval` can be defined either with a character string where `'d'`
#'   would result in daily output, and `'m'` in monthly, and `'y'` in yearly
#'   outputs. Alternatively also the as SWAT2012 input values (0 for monthly,
#'   1 for daily, 2 for yearly) are allowed.
#'
#' @param years_skip (optional) Number of years to skip before printing outputs.
#'
#'   `years_skip` can be used to define the start year in which the first
#'   simulation outputs should be written. Hence, `years_skip` is useful to
#'   define a 'warm-up' period for the simulation runs for which the simulation
#'   results are not considered.
#'
#'   `years_skip` is provided as an integer value which defines the number of
#'   years to skip before writing outputs.
#'
#'   The default values of `years_skip` is `NULL`. In this case the projects'
#'   default setting for output printing is used which is defined in the input
#'   file file.cio'.
#'
#' @param rch_out_var (optional) Numeric vector of maximum \code{length = 20} for
#'   customized output of reach variables.
#'
#'   For output codes see the
#'   \href{https://swat.tamu.edu/media/69308/ch03_input_cio.pdf}{SWAT I/O
#'   Documentation} p.77ff.
#'
#' @param sub_out_var (optional) Numeric vector of maximum \code{length = 15} for
#'   customized output of subbasin variables.
#'
#'   For output codes see the
#'   \href{https://swat.tamu.edu/media/69308/ch03_input_cio.pdf}{SWAT I/O
#'   Documentation} p.78ff.
#'
#' @param hru_out_var (optional) Numeric vector of maximum \code{length = 20} for
#'   customized output of HRU variables.
#'
#'   For output codes see the
#'   \href{https://swat.tamu.edu/media/69308/ch03_input_cio.pdf}{SWAT I/O
#'   Documentation} p.79ff.
#'
#' @param hru_out_nr (optional) Numeric vector of maximum \code{length = 20} for
#'   providing the HRU numbers for which the HRU variables are written.
#'
#'   Optional if \code{hru_out_nr = 'all'}, HRU variables are written for all HRU
#'   (caution, very large output files possible!)
#'
#' @param run_index (optional) Vector of IDs of parameter combinations for which
#'   simulations should be performed.
#'
#'   `run_index` is useful to only perform a subset of simulations for a defined
#'   parameter set (e.g. when distributing simulations between several computers,
#'   or when rerunning failed simulations for certain parameter combinations).
#'
#'   `run_index` is defined as a numeric vector where the values correspond to
#'   the row IDs of the parameter input table. (e.g.\code{run_index = c(1:100,
#'   110, 115)}). The default value is `NULL`. Then the simulations for all
#'   provided parameter combinations are performed.
#'
#' @param run_path (optional) Path to a location where the simulations should be
#'   performed.
#'
#'   `run_path` is useful if the project folder (in `project_path`) is in a
#'   location where the simulations should not be performed (e.g. because of
#'   limited space, or a virtual or slow drive). In such a case it can help to
#'   perform the simulations on a different hard drive (e.g. local drive).
#'
#'   In the path which is defined with `run_path` the folder '.model_run' is
#'   generated and the SWAT project is copied there into a number of sub-folders
#'   'thread_<i>', depending on the number of parallel runs which were defined.
#'
#'   With the default value of `run_path = NULL` the folder '.model_run' is
#'   generated in the `project_path`.
#'
#' @param n_thread (optional) Number parallel threads in which the simulations
#'   are performed. Default `n_thread` is `NULL` and all simulations are run in
#'   the folder '.model_run/thread_1' and on a single core. If `n_thread` is
#'   defined with a number greater than 1 multiple thread folders are generated
#'   and the simulations are performed in parallel on that number of cores.
#'   `n_thread` is always limited by the number of cores of the used computer.
#'
#' @param save_file (optional) Name of the folder where simulation runs should be
#'   saved incrementally.
#'
#'   If a `save_file` is provided a folder with that name is generated and the
#'   simulation results are incrementally saved in several SQLite data bases in
#'   the folder. This option is useful when very long simulation experiments are
#'   performed and computer/simulation crashes may result in the loss of days or
#'   weeks of simulation runs, or when simulation outputs are returned which
#'   would not fit into the computers RAM.
#'
#'   Default is `NULL` and simulations are not saved incrementally.
#'
#' @param save_path (optional) Path where the `save_file` folder should be
#'   generated.
#'
#'   In the default case `save_path = NULL` the `save_file` folder is generated
#'   in the `project_path`. If a `save_path` is provided the `save_file` folder
#'   is generated there if `save_file` in not `NULL`.
#'
#' @param return_output (optional) Return outputs in `R`. If `TRUE` (default
#'   value) then the defined simulated output variables are returned at the end
#'   of all simulation runs. If `FALSE` no simulations are returned in `R`.
#'
#'   **Caution:** Please only set `return_output = FALSE` if the simulations are
#'   saved in a `save_file`. Otherwise the simulations are performed but no
#'   simulations are returned.
#'
#' @param add_date (optional) Add a date vector to simulated time series. If
#'   `TRUE` (default value) a date vector is added.
#'
#' @param add_parameter (optional)  Add the used parameter changes in the
#'   simulation outputs. If `TRUE` (default value and recommended) the returned
#'   list with the simulation outputs includes the list element `.$parameter`
#'   which provides the definition of the parameters `.$parameter$definition`
#'   and the values of the parameter changes `.$parameter$values`
#'
#' @param refresh (optional) Rewrite existing '.model_run' folder. If `TRUE`
#'   (default value and recommended) always forces that .model_run' is newly
#'   written when SWAT run ins started.
#'
#' @param keep_folder (optional) Keep the thread folders in '.model_run' after
#'   finishing the simulation runs. If `FALSE` (default value) the '.model_run'
#'   folder is deleted after the last simulation. Keeping the thread folders
#'   (`keep_folder = TRUE`) can be useful to manually check input and output
#'   files in the thread folders after the simulation.
#'
#' @param quiet (optional) Printing the progress of the simulations. If
#'   `TRUE` (default value) the progress of the simulation runs is printed in the
#'   console.
#'
#' @returns
#' Returns the simulation results as list with the following elements:
#'
#' - `.$parameter` is only available if parameter changes were implemented in
#'   the simulation runs. `.$parameter` is a list with 2 elements:
#'      * `.$parameter$definition` is a tibble which shows how the parameter
#'      changes were defined.
#'      * `.$parameter$values` is a tibble with the values of the defined
#'        parameter changes. Each row is a parameter set. The names of the
#'        columns correspond to the `par_name` which are defined in
#'        `.$parameter$definition`.
#' - `.$simulation` is a list with the defined and simulated output variables.
#'   Each list element is one defined output variable. The name of each list
#'   element is the name which was defined with `define_output()`.
#' - `.$error_report` is only available if simulation runs failed. This element
#'   is a tibble which summarizes the failed simulations and the triggered errors
#'   of the SWAT executable which caused a simulation to fail.
#' - `.$run_info` is a list with meta information on the simulation run. The
#'   saved information is:
#'     * `.$run_info$simulation_log` is a tibble which logs simulation start and
#'       end time_stamps, duration and paths of the project folder.
#'    * `.$run_info$simulation_period` is a tibble which provides dates, and skipped
#'      years of the saved output variables.
#'    * `.$run_info$output_definition` is a tibble which summarizes the defined
#'      output variables which were passed with the input argument `output` and
#'      defined with `define_output()`.
#'
#' @examples
#' # Install the SWATdata R package which provides a SWAT+ demo project
#' if(!'SWATdata' %in% installed.packages()) {
#'   remotes::install_github('chrisschuerz/SWATdata')
#' }
#'
#' # Use a temporary dir for the demo project
#' tmp_dir <- tempdir()
#'
#' # Load a SWAT2012 demo project
#' proj_path <- load_demo(dataset = 'project',
#'                        path = tmp_dir,
#'                        version = '2012')
#'
#' # Perform simulations for the demo project and return
#' # daily simulated discharge for channel 1
#' q_sim <- run_swat2012(project_path = proj_path,
#'                       output = define_output(file = 'rch',
#'                                              variable = 'FLOW_OUT',
#'                                              unit = 1))
#'
#' # The simulated time series in a tibble format
#' q_sim$simulation$FLOW_OUT
#'
#' # Plot the simulated time series
#' plot(q_sim$simulation$FLOW_OUT, type = 'l')
#'
#' # Print the meta information for the simulation run
#' q_sim$run_info
#'
#' # Delete demo project folder
#' unlink(tmp_dir, recursive = TRUE, force = TRUE)
#'
#'
#' @importFrom doSNOW registerDoSNOW
#' @importFrom dplyr mutate %>%
#' @importFrom foreach foreach %dopar%
#' @importFrom lubridate now
#' @importFrom parallel detectCores makeCluster parSapply stopCluster
#' @importFrom processx run
#' @importFrom purrr map
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @export

run_swat2012 <- function(project_path, output, parameter = NULL,
                         start_date = NULL, end_date = NULL,
                         output_interval = NULL, years_skip = NULL,
                         rch_out_var = NULL, sub_out_var = NULL,
                         hru_out_var = NULL, hru_out_nr = NULL,
                         run_index = NULL, run_path = NULL,
                         n_thread = NULL, save_file = NULL,
                         save_path = NULL, return_output = TRUE,
                         add_parameter = TRUE, add_date = TRUE,
                         split_units = FALSE, refresh = TRUE,
                         keep_folder = FALSE, quiet = FALSE) {

#-------------------------------------------------------------------------------
  # Check settings before starting to set up '.model_run'
  ## General function input checks
  stopifnot(is.character(project_path))
  stopifnot(is.character(run_path)|is.null(run_path))
  stopifnot(is.numeric(n_thread)|is.null(n_thread))
  stopifnot(is.logical(add_parameter))
  stopifnot(is.logical(add_date))
  stopifnot(is.logical(split_units))
  stopifnot(is.logical(return_output))
  stopifnot(is.logical(refresh))
  stopifnot(is.logical(keep_folder))
  stopifnot(is.logical(quiet))

  ## Check if all parameter names exist in the Absolute_SWAT_Value.txt
  if(!is.null(parameter)) {
    parameter <- format_swat2012_parameter(parameter, '2012')
    file_meta <- read_file_meta(project_path, parameter$definition)
    swat_parameter <- read_swat2012_files(project_path,file_meta)
  }

  ## Check values provided with run_index and prepare run_index for simulation
  if(!is.null(run_index)){
    run_index <- check_run_index(run_index, parameter$values)
  } else {
    run_index <- 1:max(nrow(parameter$values), 1)
  }

  ## Set the .model_run folder as the run_path
  if (is.null(run_path)) {
    run_path <- paste0(project_path, '/.model_run')
  } else {
    run_path <- paste0(run_path, '/.model_run')
  }

  ## Convert output to named list in case single unnamed output was defined
  output <- prepare_output_definition(output, "2012", project_path)

  ## Read and modify the projects' file.cio, internal variable checks done.
  model_setup <- setup_swat2012(project_path, output,
                                start_date, end_date,
                                output_interval, years_skip,
                                rch_out_var, sub_out_var,
                                hru_out_var, hru_out_nr)

  # Check if weather inputs accord with start and end date
  check_dates(project_path, model_setup)

  run_info <- initialize_run_info(model_setup, output, project_path, run_path)

  ## Define save_path and check if planned simulations already exist in save file
  if(!is.null(save_file)) {
    save_path <- set_save_path(project_path, save_path, save_file)
    run_info <- initialize_save_file(save_path, parameter, run_info, run_index)
  }

#-------------------------------------------------------------------------------
  # Build folder structure where the model will be executed
  ## Identify the required number of parallel threads to build.
  n_thread <- min(max(nrow(parameter$values),1),
                  max(n_thread,1),
                  max(length(run_index),1),
                  detectCores())

  ## Identify operating system and find the SWAT executable in the project folder
  os <- get_os()

  ## Manage the handling of the '.model_run' folder structure.
  swat_exe <- manage_model_run(project_path, run_path, n_thread, os,
                               "2012", refresh, quiet)
#-------------------------------------------------------------------------------
  # Write files
  ## Write file.cio
  write_file_cio(run_path, model_setup$file.cio)
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
  t0 <- now()

  ## Initialize the save_file if defined
  if(!is.null(save_file)) {
    initialize_save_file(save_path, parameter, run_info)
  }

  if(!quiet) {
    cat("Performing", n_run, ifelse(n_run == 1, "simulation", "simulations"),
        "on", n_thread, "cores:", "\n")
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
      model_output <- read_swat2012_output(output, thread_path)

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
  }

  n_digit <- get_digit(parameter$values)
  sim_result <- set_names(sim_result,
                          "run"%_%sprintf("%0"%&%n_digit%&%"d", run_index))

  run_info <- add_run_info(run_info, sim_result, run_index)

  if(!is.null(save_file)) {
    update_sim_log(save_path, run_info)
  }

  ## Delete the parallel threads if keep_folder is not TRUE
  if(!keep_folder) unlink(run_path, recursive = TRUE)

  if("error_report" %in% names(sim_result)) {
    warning("Some simulations runs failed! Check '.$error_report' in your",
            " simulation results for further information.")
  }

  ##Tidy up and return simulation results if return_output is TRUE
  if(return_output) {
    output_list <- list()

    if(add_parameter) {
      output_list$parameter <- parameter[c('values', 'definition')]
    }

    output_list$simulation <- tidy_simulations(sim_result)

    if(add_date) {
      ## Create date vector from the information in model_setup
      date <- get_date_vector_2012(model_setup)
      output_list$simulation <- map(output_list$simulation, ~ bind_cols(date, .x))
    }

    output_list$error_report <- prepare_error_report(sim_result)

    output_list$run_info <- run_info

    return(output_list)
  }
}
