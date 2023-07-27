#' Run a set of scenarios for a SWAT+/SWAT2012 project
#'
#' @description
#' `run_scenario()` uses scenario input files which are organized in the
#' `scenario_path` and performs scenario simulations for a SWAT+/SWAT2012 model
#' setup, which is located in the `project_path`. The scenario simulations
#' return output variables which are defined with `output`. The version of the
#' model set up is defined with `version`. Scenarios can be run in parallel by
#' defining `n_thread`.
#'
#' Further input arguments for `run_swatplus()` and `run_swat2012()` can be
#' passed to `run_scenario()`. E.g. if a parameter set is provided with
#' `parameter` all defined parameter combinations will be run for each scenario.
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
#' @param n_thread (optional) Number parallel threads in which the simulations
#'   are performed. Default, `n_thread` is `NULL` and all simulations are run on
#'   a single core. If `n_thread` is defined with a number greater than 1 the
#'   simulations are performed in parallel on that number of cores.
#'   `n_thread` is always limited by the number of cores of the used computer.
#'
#' @param ... Other input arguments passed on to `run_swatplus()` or
#'   `run_swat2012()` depending on the `version` of the SWAT project. All input
#'   arguments passed with `...` must be defined for the respective run function.
#'
#' @examples
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
#'
run_scenario <- function(project_path, scenario_path, output, version,
                         n_thread = NULL, ...) {

#-------------------------------------------------------------------------------
  # General function input checks
  stopifnot(is.character(project_path))
  stopifnot(is.character(scenario_path))
  stopifnot(is.list(output))
  stopifnot(is.numeric(n_thread)|is.null(n_thread))
  stopifnot(version %in% c('plus', '2012'))

# ------------------------------------------------------------------------------
  # Define the number of iterations and parallel threads for scenarios and
  # parameters.

  # Only get names of sub folders of scenario_path
  is_dir <- file.info(dir(scenario_path,full.names=T))$isdir
  scenario_name <- dir(scenario_path)[is_dir]

  # Number of scenarios
  n_scenario <- length(scenario_name)

  # Get the names of all input arguments including the ones passed with ...
  arg_names <- names(as.list(match.call()))
  # Check if not supported input arguments were provided by the user.
  check_arg_names(arg_names, version)

  n_parameter <- get_n_parameter(...)

  # Define the optimum combination of parallel threads for scenarios and
  # parameters with the available number of threads to minimize the number of
  # required parallel model iterations.
  n_parallel <- get_n_parallel(n_thread, n_scenario, n_parameter)

  if(n_parallel['parameter'] == 1) {
    # If parameters are not run in parallel simulations will be run directly
    # in the scenario folders
    run_in_project <- TRUE
  } else {
    run_in_project <- FALSE
  }

# ------------------------------------------------------------------------------
# Next here generate parallel folder structure for scenarios which will then be
# used by the workers to run simulations.


#-------------------------------------------------------------------------------
  # Initiate foreach loop to run SWAT models
  ## make and register cluster, create table that links the parallel worker
  ## with the created parallel thread folders in '.model_run'
  cl_scen <- makeCluster(n_parallel['scenario'])
  worker <- tibble(worker_id = parSapply(cl, 1:n_thread,
                                         function(x) paste(Sys.info()[['nodename']],
                                                           Sys.getpid(), sep = "-")),
                   thread_id = dir(run_path) %>% .[grepl("thread_",.)])

  registerDoSNOW(cl_scen)
}
