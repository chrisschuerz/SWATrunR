#' Translate the parameter inputs into a parameter input table and a separarate
#' table providing the file constraints and the filter expressions for the
#' respective parameter
#'
#' @param parameter Model parameters as named vector or tibble
#'
#' @keywords internal
#'
format_swatplus_parameter <- function(parameter) {
  par_constrain <- suppressWarnings(translate_parameter_constraints(names(parameter)))
  par_constrain <- select(par_constrain, -file_expression, -spec_expression)
  names(parameter) <- par_constrain$par_name
  if(!is.data.frame(parameter)) parameter <- map_dfc(parameter, ~.x)
  return(list(values = parameter, parameter_constrain = par_constrain))
}

write_calibration <- function(thread_path, parameter, run_files, i_run) {

}
