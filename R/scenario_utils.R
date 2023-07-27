#' Get the number of parameter sets which shold be run for each scenario.
#'
#' @param parameter Parameter input table that is passed as an optional
#'   `run_swat*()` input argument.
#' @param run_index Indices of parameter combintations which should be run
#'   from the parameter set in `parameter`. This is an optional input argument
#'   passed on to `run_swat*()`.
#' @param ... Addtional input arguments which can be passed on. Not considered
#'   in this function.
#'
#' @returns the number of parameter sets as integer value
#'
#' @keywords internal
#'
get_n_parameter <- function(parameter = NULL, run_index = NULL, ...) {
  # Get the number of parameter sets which should be run for each scenario based
  # on the additionally provided input arguments
  if (!is.null(parameter)) {
    # If run_index is provided n_parameter is the number of selected parameter
    # sets. Otherwise the rows of parameter or if it is a vector set to 1.
    if(!is.null(run_index)) {
      run_index <- check_run_index(run_index, parameter)
      n_parameter <- length(run_index)
    } else {
      n_parameter <- max(1, nrow(parameter))
    }
  } else {
    # If no parameter set is provided set to 1
    n_parameter <- 1
  }

  return(n_parameter)
}

#' Get numbers of parallel threads for scenarios and parameters which minimize
#' the number of parallel iterations.
#'
#' @param n_thread Number of parallel threads defined by the user.
#' @param n_scenario Number of scenarios which should be run.
#' @param n_parameter Number of parameter sets which should be run for every
#'   scenario.
#'
#' @returns A vector of length 2 with the number of parallel threads for
#'   scenarios and parameters and the the total number of iterations.
#'
#' @importFrom dplyr filter mutate %>%
#' @importFrom parallel detectCores
#'
#' @keywords internal
#'
get_n_parallel <- function(n_thread, n_scenario, n_parameter) {
  n_thread <- min(detectCores(), max(n_thread, 1))

  n_comb <- expand.grid(n_s = 1:n_thread, n_p = 1:n_thread) %>%
    mutate(n_c = n_s*n_p) %>%
    filter(n_c <= n_thread) %>%
    mutate(i_s = ceiling(n_scenario  / n_s),
           i_p = ceiling(n_parameter / n_p),
           i_c = i_s * i_p) %>%
    filter(i_c == min(i_c)) %>%
    filter(n_p == min(n_p)) %>%
    filter(n_c == min(n_c))

  n_parallel <- c(scenario   = n_comb$n_s,
                  parameter  = n_comb$n_p,
                  iterations = n_comb$i_c)

  return(n_parallel)
}

#' Check the names of the passed input arguments and trigger error if argument
#' is not supported by any of the run functions.
#'
#' @param arg_names Character vector of provided input arguments
#' @param version Version of the SWAT project, one of 'plus', '2012'
#'
#' @returns Error message if argument is not supported
#'
#' @keywords internal
#'
check_arg_names <- function(arg_names, version) {
  # Define possible input arguments for both SWAT+ and SWAT2012
  names_gen <- c('project_path', 'scenario_path', 'output', 'version', 'n_thread',
                 'parameter', 'start_date', 'end_date', 'years_skip', 'run_index',
                 'run_path', 'save_file', 'save_path', 'return_output',
                 'add_parameter', 'add_date', 'split_units', 'keep_folder' ,'')

  # SWAT+ specific input arguments
  names_plus <- c('start_date_print', 'time_out')
  names_2012 <- c('output_interval', 'rch_out_var', 'sub_out_var', 'hru_out_var',
                  'hru_out_nr')

  # Input arguments which will be defined by run_scenario()
  names_scen <- c('quiet', 'refresh', 'run_in_project')

  if (any(arg_names %in% names_scen)) {
    stop('The input arguments', paste(names_scen, collapse = ', '), 'are not ',
         "supported by 'run_scenario()'.")
  }

  if (version == 'plus') {
    names_check <- c(names_gen, names_plus)
  } else {
    names_check <- c(names_gen, names_2012)
  }

  not_in_names <- ! arg_names %in% names_check

  if(any(not_in_names)) {
    args_not_in_names <- arg_names[not_in_names]

    stop('The following input arguments passed with ... are not supported by ',
         ifelse(version == 'plus', "'run_swatplus()'", "'run_swat2012()'"),
         ':\n',
         paste(args_not_in_names, collapse = ', '))
  }
}
