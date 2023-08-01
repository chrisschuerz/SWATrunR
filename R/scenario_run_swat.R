#' Run a set of scenarios for a SWAT+/SWAT2012 project
#'
#' @description
#' `run_scenario()` performs scenario simulations for a SWAT+ or SWAT2012 model
#' setup, which is located in the `project_path`. Scenarios are defined by SWAT
#' sub-folders in the `scenario_path`. The name of each scenario is defined by
#' its sub-folder name. SWAT input files in each scenario folder define a scenario.
#' These are input files (e.g. weather input files) which overwrite the respective
#' input files in a copy of the original SWAT model setup defined in `project_path`.
#'
#' The scenario simulations return output variables which are defined with
#' `output`. The version of the model set up is defined with `version`.
#' `version` defines whether `run_swatplus()` (`version = 'plus'`), or
#' `run_swat2012()` (`version = '2012'`) is called. Scenarios can be run in parallel by
#' defining `n_thread`.
#'
#' Further input arguments for `run_swatplus()` and `run_swat2012()` can be
#' passed to `run_scenario()`. E.g. if a parameter set is provided with
#' `parameter` all defined parameter combinations will be run for each scenario.
#' Simulations can e.g. be run in parallel by defining `n_thread`. For a full
#' list of input arguments see the help pages of \code{\link{run_swatplus}} and
#' \code{\link{run_swat2012}}.
#'
#' @param project_path  Path to the SWAT project folder on the hard drive
#'   (i.e. txtinout folder).
#'
#' @param scenario_path Path to the folder on the hard drive in which the
#'   scenarios are organized. The scenarios must be defined in sub-folders. All
#'   files of a sub-folder will overwrite the respective files of the SWAT
#'   project. Differences between the original files and the scenario files
#'   define a scenario. The scenario outputs will have the same name as the
#'   respective scenario folders.
#'
#' @param output Output variables to extract and return for each SWAT model run.
#'
#'   Each output variable which should be returned is defined with
#'   \code{\link{define_output}}.
#'   If multiple output variables should be returned for a model run, the output
#'   definitions must be concatenated in a named list. See the examples provided
#'   on the help page of the functions \code{\link{run_swatplus}} and
#'   \code{\link{run_swat2012}} and on the help page of
#'   \code{\link{define_output}}.
#'
#' @param version The version of the SWAT model setup in `project_path`.
#'   `version` must be either `'plus'` for SWAT+ model setups and `'2012'` if
#'   the model setup is a SWAT2012 project.
#'
#' @param ... Other input arguments passed on to `run_swatplus()` or
#'   `run_swat2012()` depending on the `version` of the SWAT project. All input
#'   arguments passed with `...` must be defined for the respective run function.
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
#'   Each list element is a scenario output. The name of the list element is the
#'   same as the scenarios folder name. Each scenario element is a list where
#'   each list element is one defined output variable. The name of each list
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
#'
#' @importFrom doSNOW registerDoSNOW
#' @importFrom dplyr mutate %>%
#' @importFrom foreach foreach %dopar%
#' @importFrom lubridate now
#' @importFrom parallel detectCores makeCluster parSapply stopCluster
#' @importFrom purrr list_rbind map map_lgl map2 set_names
#' @importFrom tibble tibble
#'
#' @export
#'
run_scenario <- function(project_path, scenario_path, output, version, ...) {

#-------------------------------------------------------------------------------
  # General function input checks
  stopifnot(is.character(project_path))
  stopifnot(is.character(scenario_path))
  stopifnot(is.list(output))
  stopifnot(version %in% c('plus', '2012'))

# ------------------------------------------------------------------------------
  # Define the number of iterations and parallel threads for scenarios and
  # parameters.

  # Get the all input arguments and names including the ones passed with ...
  dot_args <- list(...)

  # Only get names of sub folders of scenario_path
  is_dir <- file.info(dir(scenario_path,full.names=T))$isdir
  scenario_name <- dir(scenario_path)[is_dir]

  # Number of scenarios
  n_scenario <- length(scenario_name)


  n_parameter <- get_n_parameter(parameter = dot_args$parameter,
                                 run_index = dot_args$run_index)

  # Define the optimum combination of parallel threads for scenarios and
  # parameters with the available number of threads to minimize the number of
  # required parallel model iterations.
  n_parallel <- get_n_parallel(dot_args$n_thread, n_scenario, n_parameter)

  if(n_parallel['parameter'] == 1) {
    # If parameters are not run in parallel simulations will be run directly
    # in the scenario folders
    run_in_project <- TRUE
  } else {
    run_in_project <- FALSE
  }

  if (is.null(dot_args$run_path)) {
    run_path <- paste0(project_path, '/.scenario_run')
  } else {
    run_path <- paste0(dot_args$run_path, '/.scenario_run')
  }

  if(is.null(dot_args$return_output)) dot_args$return_output <- TRUE
  if(is.null(dot_args$add_parameter)) dot_args$add_parameter <- TRUE
  if(is.null(dot_args$keep_folder))   dot_args$keep_folder <- FALSE
  if(!is.null(dot_args$run_path))     dot_args$run_path <- NULL
  if(!is.null(dot_args$n_thread))     dot_args$n_thread <- NULL
  if (!is.null(dot_args$quiet)) {
    quiet <- dot_args$quiet
    dot_args$quiet <- NULL
  } else {
    quiet <- FALSE
  }

  # Check if not supported input arguments were provided by the user.
  check_arg_names(dot_args, version)

#-------------------------------------------------------------------------------
  # Initiate foreach loop to run SWAT models
  ## make and register cluster, create table that links the parallel worker
  ## with the created parallel thread folders in '.model_run'
  ##
  if(!quiet) {
    cat(n_scenario ,'Scenario folder'%&%plural(n_scenario),
        'with the following name'%&%plural(n_scenario),
        "found in 'scenario_path':\n",
        paste0(scenario_name, collapse = ', '), '\n\n')
    if('parameter' %in% names(dot_args)) {
      cat(n_parameter, 'parameter set'%&%plural(n_parameter), 'defined to be',
          'used in each scenario simulation.\n\n')
    }
    cat("Performing in total", n_scenario*n_parameter,
        "simulation"%&%plural(n_scenario*n_parameter),
        "on", n_parallel['core_total'],
        "core"%&%plural(n_parallel['core_total'])%&%".", "\n")
    if(n_parallel['scenario'] == 1) {
      cat('Scenarios will be run sequentially on 1 core in', run_path%&%'.\n')
    } else {
      cat('Scenarios will be run in parallel on', n_parallel['scenario'],
          'cores in', run_path%&%'.\n')
    }
    if(!'parameter' %in% names(dot_args)) {
      cat('')
    } else if(n_parallel['parameter'] == 1) {
      cat('The defined parmeter changes will be implemented directly in the',
          'scenario thread folder'%&%plural(n_parallel['scenario'])%&%'.\n')
    } else {
      cat('In each scenario thread folder', n_parallel['parameter'],
          "thread folders will be generated in '.model_run/thread_<i>'",
          'for parallel parameter simulations.\n')
    }

    cat('\nPerforming simulations:\n')

    progress <- function(n){
      display_progress(n, n_scenario, t0, "Scenario")
    }
    opts <- list(progress = progress)
  } else {
    opts <- list()
  }

  thread_ids <- build_scenario_run(project_path, run_path, n_parallel)

  cl_scen <- makeCluster(n_parallel['scenario'])
  worker <- tibble(worker_id = parSapply(cl_scen, 1:n_parallel['scenario'],
                                         function(x) paste(Sys.info()[['nodename']],
                                                           Sys.getpid(), sep = "-")),
                   thread_id = thread_ids)

  registerDoSNOW(cl_scen)

  ## If not quiet a function for displaying the simulation progress is generated
  ## and provided to foreach via the SNOW options
  t0 <- now()

  sim_result <- foreach(i_scn = 1:n_scenario,
                        .packages = c("dplyr", "lubridate", "processx", "stringr"),
                        .options.snow = opts) %dopar% {
    # for(i_scn in 1:n_scenario) {
    ## Identify worker of the parallel process and link it with respective thread
    worker_id <- paste(Sys.info()[['nodename']], Sys.getpid(), sep = "-")
    thread_id <- worker[worker$worker_id == worker_id, 2][[1]]
    thread_path <- run_path%//%thread_id
    # thread_path <- run_path%//%'thread_1'
    args_list  <- list(project_path = thread_path,
                       output       = output,
                       n_thread     = n_parallel['parameter'],
                       quiet = TRUE,
                       run_in_project = run_in_project)
    args_list <- c(args_list, dot_args)

    scn_i <- scenario_name[i_scn]

    thread_files <- list.files(thread_path, full.names = TRUE)
    file.remove(thread_files)

    project_files <- list.files(project_path, full.names = TRUE)
    file.copy(project_files, thread_path)

    scenario_files <- list.files(paste0(scenario_path, '/', scn_i),
                                 full.names = TRUE)
    file.copy(scenario_files, thread_path, overwrite = TRUE)

    if(version == 'plus') {
      scn_sim <- do.call(run_swatplus, args_list)
    }else {
      scn_sim <- do.call(run_swat2012, args_list)
    }

    if(dot_args$return_output) {
      return(scn_sim)
    }
  }

  ## Stop cluster after parallel run
  stopCluster(cl_scen)

  if (!quiet) {
    finish_progress(n_scenario, t0, "scenario")
  }

  ## Delete the parallel threads if keep_folder is not TRUE
  if(!dot_args$keep_folder) unlink(run_path, recursive = TRUE)

  if (dot_args$return_output) {
    parameter <- sim_result[[1]]$parameter

    has_sim <- map_lgl(sim_result, ~! is.null(.x$simulation))

    scn_simulations <- sim_result[has_sim] %>%
      map(., ~.x$simulation) %>%
      set_names(., scenario_name[has_sim])

    sim_log <- sim_result %>%
      map(., ~.x$run_info$simulation_log) %>%
      map2(., scenario_name, ~ mutate(.x, scenario = .y, .before = 1)) %>%
      list_rbind(.) %>%
      mutate(run_path = project_path) %>%
      mutate(scenario_path = scenario_path, .before = run_path)
    sim_log$project_path <- project_path

    run_info <- list(simulation_log = sim_log,
                     simulation_period = sim_result[[1]]$run_info$simulation_period,
                     output_definition = sim_result[[1]]$run_info$output_definition)

    error_report <- map(sim_result, ~.x$error_report)
    has_error    <- map_lgl(error_report, ~ !is.null(.x))

    if(any(has_error)) {
      warning("Some simulations runs failed! Check '.$error_report' in your",
              " simulation results for further information.")
      scn_err_name <- scenario_name[has_error]

      error_report <- sim_result[has_error] %>%
        map(., ~.x$error_report) %>%
        map2(., scn_err_name, ~ mutate(.x, scenario = .y, .before = 1)) %>%
        list_rbind(.)
    } else {
      error_report <- NULL
    }

    output_list <- list()

    if(dot_args$add_parameter) {
      output_list$parameter <- parameter
    }
    output_list$simulation <- scn_simulations
    output_list$error_report <- error_report
    output_list$run_info <- run_info

    ## ...and return simulation results if return_output is TRUE
    return(output_list)
  }
}
