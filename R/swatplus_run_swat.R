#' Run simulations for a SWAT+ project
#'
#' @description
#' `run_swatplus()` performs simulations for a SWAT+ model setup that is located
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
#'   \href{https://chrisschuerz.github.io/SWATrunR/articles/SWATrunR.html#changing-parameter-values}{`SWATrunR`}
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
#' @param years_skip,start_date_print (optional) Start year/date for output printing.
#'
#'   `years_skip` or `start_date_print` can be used to define the start year/date
#'   in which the first simulation outputs should be written. The two arguments
#'   are useful to define a 'warm-up' period for the simulation runs for which
#'   the simulation results are not considered.
#'
#'   `years_skip` is provided as an integer value which defines the number of
#'   years to skip before writing outputs.
#'
#'   `start_date_print` can be provided as character string in any ymd format
#'   (e.g. 'yyyy-mm-dd'), as a numeric value (yyyymmdd) or in a `Date`
#'   format.
#'
#'   For the simulation of hydrological years (which do not start at Jan 1.)
#'   the use of `start_date_print` is the preferred option to define the actual
#'   start date of the hydrological year (e.g. '2000-10-01').
#'
#'   `start_date_print` is always overruled by the definition of `years_skip`.
#'
#'   The default values of `years_skip` and `start_date_print` are `NULL`.
#'   If both are `NULL` the projects' default setting for output printing which
#'   is defined in the input file 'print.prt' is used.
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
#' @param add_parameter (optional)  Add the used parameter changes in the
#'   simulation outputs. If `TRUE` (default value and recommended) the returned
#'   list with the simulation outputs includes the list element `.$parameter`
#'   which provides the definition of the parameters `.$parameter$definition`
#'   and the values of the parameter changes `.$parameter$values`.
#'
#' @param add_date (optional) Add a date vector to simulated time series. If
#'   `TRUE` (default value) a date vector is added.
#'
#' @param split_units (optional) Split simulated outputs of the same variable into
#'   separate tables in the `.$simulation` list. If `TRUE` (default value) outputs
#'   are split (e.g. for `define_output('channel_sd_day', 'flo_out', 1:3)`) the
#'   output tables `flo_out_1`, `flo_out_2`, and `flo_out_3` will be generated.
#'   If `FALSE` in the same example a single table is generated with an integer
#'   column `unit` with the ID values 1 to 3.
#'
#' @param run_in_project Should a simulation be executed directly in the project
#'   folder? If `FALSE` (default) the subfolder structure '.model_run/thread_*'
#'   is generated and simulations are run in the thread folders. If `TRUE` the
#'   simulation will be performed directly in the `project_path`. This is only
#'   allowed for single simulation runs (No or a single parameter set provided).
#'   **Caution:** This option can overwrite the original model input files!
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
#' @param time_out (optional) Timeout for simulation runs in seconds.
#'
#'   Simulations may hang up due to specific parameter combinations. A timeout
#'   kills any simulation if the run time exceeds the set time in seconds.
#'   Be careful with this setting. A timeout which is set shorter than the
#'   actual run time of a simulation will also kill all potentially successful
#'   runs before finishing.
#'
#'   Default `time_out` is set to `Inf` to not kill any simulations.
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
#' @section Examples for output definition:
#'
#' Outputs are defined with the function `define_output()`. The function has the
#' three input arguments `file` which defines the output file into which the
#' variable of interest is written, `variable` which is the variable name as it is
#' defined in the output file (without the unit), and `unit` which is the spatial
#' unit for which the variable is extracted (e.g. channel ID, or HRU, etc., but
#' also the crop names in the case of management outputs).
#'
#' The following examples show definitions for output variables. In a simulation
#' run with `run_swatplus()` these definitions must be passed with the input
#' argument `output`.
#'
#' A single output variable can be defined like in the following examples:
#'
#' ```
#' # Define monthly basin wide ET
#' # Basin ET is in written to basin_wb.
#' # The definition of the time interval with the file name is required (here 'mon').
#' define_output(file = 'basin_wb_mon',
#'               variable = 'et', # This is the variable name in the file.
#'               unit = 1 # Basin output have unit = 1 as there is 1 basin
#'               )
#'
#' # Define daily 'flo_out' for the channels 1 to 3 and 7
#' # Channel outputs can be extracted either from 'channel_sd' or 'channel_sdmorph'.
#'  define_output(file = 'channel_sdmorph_day',
#'                variable = 'flo_out',
#'                unit = c(1:3, 7) # To define the channel IDs of interest.
#'               )
#'```
#'
#' More than one variable must be defined in a named list. The name of each
#' output definition is then assigned to the returned outputs together with the
#' unit ID if more than one ID was defined. `SWATrunR` > 1.0.0 allows to return
#' outputs with different output intervals at the same time. Further non-time
#' series outputs such as flow duration curve (FDC) outputs, average annual crop
#' yields, and annual management outputs such as crop yields, biomass, or growth
#' stress factors can also be returned now.
#'
#' ```
#' # Define the outputs for
#' # - Average annual ET for the HRUs 1 to 847
#' # - Channel discharge for the channels 1, 34, and 57
#' # - FDC outputs for the channels 1, 34, and 57
#' # - Average annual crop yields
#' # - HRU and year specific crop yields for 'corn' and 'pnut'
#' #
#' list(et_hru  = define_output('hru_wb_aa', 'et', 1:847),
#'      cha_q   = define_output('channel_sd_day', 'flo_out', c(1, 34, 57)),
#'      fdc     = define_output(file = 'fdcout', unit = c(1, 34, 57)),
#'      yld_aa  = define_output('basin_crop_yld_aa', 'yld'),
#'      yld_mgt = define_output('mgtout', 'yld', c('corn', 'pnut')))
#' ```
#'
#' @section Examples for parameter definition:
#'
#' The definition of parameter changes follows a strict syntax in the parameter
#' names which have to be assigned to the change values. Therefore it is highly
#' recommended to read the corresponding section on the
#' \href{https://chrisschuerz.github.io/SWATrunR/articles/SWATrunR.html#changing-parameter-values}{`SWATrunR`}
#' github page. The following examples show very basic parameter definitions and
#' do not cover specific naming of parameters, or parameter conditions which are
#' topics covered on the github page.
#'
#' In a nutshell the name definition of a parameter requires at least the name
#' of the parameter how it is defined in the calibration.cal input file, the
#' file suffix, and the type of change, as shown in this example:
#'
#' ```
#' # Change cn2 by -5% and alpha by an absolute value of 0.35
#' par_set <- c("cn2.hru | change = relchg" = - 0.05,
#' "           "alpha.aqu | change = absval" = 0.35)
#' ```
#'
#' As shown in the example above, single parameter changes can be passed as a
#' named vector. Multiple parameter combinations must be passed in a tibble
#' (caution here, base R data.frames might not be able to handle all special
#' characters in the parameter names). Below an example for a more comprehensive
#' parameter set with uniform randomly sampled parameter changes:
#'
#' ```
#' library(tibble)
#'
#' # Draw 100 samples for each parameter
#' n <- 100
#'
#' # Sample each parameter change uniformly n times in their upper/lower bounds
#' par_set <- tibble('cn2.hru | change = abschg' = runif(n,-15,10),
#'                   'lat_ttime.hru | change = absval' = runif(n,0.5,5),
#'                   'lat_len.hru | change = abschg' = runif(n,-10,50),
#'                   'epco.hru | change = absval' = runif(n,0.1,1),
#'                   'esco.hru | change = absval' = runif(n,0.1,1),
#'                   'perco.hru | change = absval' = runif(n,0.1,0.8),
#'                   'k.sol | change = pctchg' = runif(n,-20,100),
#'                   'awc.sol | change = pctchg' = runif(n,-20,20),
#'                   'alpha.aqu | change = absval' = runif(n,0.01,0.3))
#' ```
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
#' # Load a SWAT+ demo project
#' proj_path <- load_demo(dataset = 'project',
#'                        path = tmp_dir,
#'                        version = 'plus')
#'
#' # Perform simulations for the demo project and return
#' # daily simulated discharge for channel 1
#' q_sim <- run_swatplus(project_path = proj_path,
#'                       output = define_output(file = 'channel_sd_day',
#'                                              variable = 'flo_out',
#'                                              unit = 1))
#'
#' # The simulated time series in a tibble format
#' q_sim$simulation$flo_out
#'
#' # Plot the simulated time series
#' plot(q_sim$simulation$flo_out, type = 'l')
#'
#' # Print the meta information for the simulation run
#' q_sim$run_info
#'
#' # Delete demo project folder
#' unlink(tmp_dir, recursive = TRUE, force = TRUE)
#'
#' @importFrom data.table fread
#' @importFrom doSNOW registerDoSNOW
#' @importFrom dplyr %>%
#' @importFrom foreach foreach %dopar%
#' @importFrom lubridate now
#' @importFrom parallel detectCores makeCluster parSapply stopCluster
#' @importFrom processx run
#' @importFrom purrr map map_if map_lgl
#' @importFrom stringr str_split
#' @importFrom tibble tibble as_tibble
#' @export
run_swatplus <- function(project_path, output, parameter = NULL,
                         start_date = NULL, end_date = NULL,
                         years_skip = NULL, start_date_print = NULL,
                         run_index = NULL, run_path = NULL,
                         n_thread = NULL, save_file = NULL,
                         save_path = NULL, return_output = TRUE,
                         add_parameter = TRUE, add_date = TRUE,
                         split_units = TRUE, run_in_project = FALSE,
                         refresh = TRUE, keep_folder = FALSE,
                         quiet = FALSE, time_out = Inf) {

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
  stopifnot(is.logical(split_units))
  stopifnot(is.logical(return_output))
  stopifnot(is.logical(refresh))
  stopifnot(is.logical(keep_folder))
  stopifnot(is.logical(quiet))
  stopifnot(is.numeric(time_out))

  if(!return_output & is.null(save_file)) {
    stop("'return_output = FALSE' and no 'save_file' is defined. ",
         'Simulation runs would be performed without returning or saving ',
         'simulatied outputs!')
  }

  ## Check if all parameter names exist in cal_parms.cal and plants.plt
  if(!is.null(parameter)) {
    parameter <- format_swatplus_parameter(parameter)
    check_swatplus_parameter(project_path, parameter)
    unit_conds <- read_unit_conditions(project_path, parameter)
  ## Read the plants.plt data base if plant parameters should be adjusted.
    if('pdb' %in% parameter$definition$file_name) {
      parameter$plants_plt <-
        as_tibble(fread(paste0(project_path, '/plants.plt'),skip = 1))
    }

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

  ## Set the run_path based on the input arguments run_path, project_path, and
  ## run_in_project
  if (run_in_project) {
    if (!is.null(run_path)) {
      cat("'run_path' was ignored because 'run_in_project = TRUE'.\n\n")
    }

    run_path <- project_path

    if(length(run_index) > 1) {
      stop('Only single run with no or one parameter combination can be run ',
           "directly in the 'project_path'. To run multiple simulations, set ",
           "'run_in_project = FALSE'")
    }

  } else {
    if (is.null(run_path)) {
      run_path <- paste0(project_path, '/.model_run')
    } else {
      run_path <- paste0(run_path, '/.model_run')
    }
  }

  ## Convert output to named list in case single unnamed output was defined
  output <- prepare_output_definition(output, "plus", project_path)

  ## Read and modify the projects' files defining simulation period years to
  ## skip, interval, etc.
  model_setup <- setup_swatplus(project_path, parameter, output,
                                start_date, end_date, start_date_print,
                                years_skip, unit_conds)

  run_info <- initialize_run_info(model_setup, output, project_path, run_path)

  # Check if weather inputs accord with start and end date
  check_dates(project_path, model_setup)

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

  ## Find executable file in project_path
  swat_exe <- find_exe_file(project_path, os)

  ## Manage the handling of the '.model_run' folder structure.
  manage_model_run(project_path, run_path, n_thread, os, "plus",
                   run_in_project, refresh, quiet)

#-------------------------------------------------------------------------------
  # Write files
  ## Write model setup: Files that define the time range etc. of the SWAT
  ## simulation
  write_swatplus_setup(run_path, model_setup, run_in_project)

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

  if(!quiet) {
    cat("Performing", n_run, "simulation"%&%plural(n_run),"on", n_thread,
        "core"%&%plural(n_thread)%&%":", "\n")
    progress <- function(n){
      display_progress(n, n_run, t0, "Simulation")
    }
    opts <- list(progress = progress)
  } else {
    opts <- list()
  }

  sim_result <- foreach(i_run = 1:n_run,
   .packages = c("dplyr", "lubridate", "processx", "stringr"),
   .options.snow = opts) %dopar% {
    # for(i_run in 1:max(nrow(parameter), 1)) {
    if(run_in_project) {
      thread_path <- project_path
    } else {
      ## Identify worker of the parallel process and link it with respective thread
      worker_id <- paste(Sys.info()[['nodename']], Sys.getpid(), sep = "-")
      thread_id <- worker[worker$worker_id == worker_id, 2][[1]]
      thread_path <- run_path%//%thread_id
    }
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
      # parameter <- parameter[c('values', 'definition')]
    }

    ## Execute the SWAT exe file located in the thread folder
    msg <- run(run_os(swat_exe, os), wd = thread_path,
               error_on_status = FALSE, timeout = time_out)

    if(msg$timeout) {
      out_msg <- str_split(msg$stdout, '\r\n|\r|\n', simplify = TRUE) %>%
        .[max(1, length(.) - 10):length(.)]
      err_msg <- c('Error:', paste0('Simulation timed out after ', time_out, ' sec'),
                   'Simulation run:', out_msg)
      model_output <- err_msg
      if(!is.null(save_path)) {
        save_error_log(save_path, model_output, parameter, run_index, i_run)
        # update_run_log(save_path, run_index[i_run], 'time_out')
      }
    } else if(nchar(msg$stderr) == 0) {
      model_output <- read_swatplus_output(output, thread_path, add_date, split_units)

      if(!is.null(save_path)) {
        save_run(save_path, model_output, parameter, run_index, i_run, thread_id)
        # update_run_log(save_path, run_index[i_run], 'finished')
      }
    } else {
      out_msg <- str_split(msg$stdout, '\r\n|\r|\n', simplify = TRUE) %>%
        .[max(1, length(.) - 10):length(.)]
      err_msg <- str_split(msg$stderr, '\r\n|\r|\n', simplify = TRUE)
      err_msg <- c('Last output:', out_msg, 'Error:', err_msg)
      model_output <- err_msg
      if(!is.null(save_path)) {
        save_error_log(save_path, model_output, parameter, run_index, i_run)
        # update_run_log(save_path, run_index[i_run], 'error')
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
  if(!keep_folder & ! run_in_project) unlink(run_path, recursive = TRUE)

  if("error_report" %in% names(sim_result)) {
    warning("Some simulations runs failed! Check '.$error_report' in your",
            " simulation results for further information.")
  }
  ##Tidy up results if return_output is TRUE
  if(return_output) {
    output_list <- list()


    if(add_parameter) {
      output_list$parameter <- parameter[c('values', 'definition')]
    }
    output_list$simulation <- tidy_simulations(sim_result)
    output_list$error_report <- prepare_error_report(sim_result)
    output_list$run_info <- run_info
    ## ...and return simulation results if return_output is TRUE
    return(output_list)
  }
}
