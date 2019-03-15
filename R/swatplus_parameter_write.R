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
  return(list(values = parameter, definition = par_constrain))
}


#' Update the calibration file structure with the parameter set of the current
#' simulation run_i
#'
#' @param thread_path Path to the current parallel thread 'thread_i'
#' @param parameter Model parameters as named vector or tibble
#' @param calibration Template table structure of the calibration file
#' @param i_run Index of the i_th simulation run
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map2_df map_dbl
#' @importFrom readr write_lines
#'
#' @keywords internal
#'
write_calibration <- function(thread_path, parameter, calibration, run_index,
                              i_run) {
  calibration$VAL <- parameter$values[run_index[i_run],] %>%
    map_dbl(., ~.x) %>%
    .[calibration$NAME] %>%
    sprintf("%.15s", .)

  col_format <- c("%-8s", "%8s", "%16s", rep("%8s", 8))

  col_names <- names(calibration) %>%
    sprintf(col_format, .) %>%
    paste(., collapse = "")

  calibration <- map2_df(calibration, col_format, ~sprintf(.y, .x)) %>%
    apply(., 1, paste, collapse = "") %>%
    c("Number of parameters:", sprintf("%2d",length(.)), col_names, .)

  write_lines(calibration, thread_path%//%"calibration.cal")
}


#' Check if the names of the defined parameters are available in 'cal_parms.cal'.
#'
#' @param project_path Path to the SWAT+ project
#' @param parameter Model parameter data set
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @importFrom readr read_lines
#'
#' @keywords internal
#'
check_swatplus_parameter <- function(project_path, parameter) {
  if("cal_parms.cal" %in% list.files(project_path)) {
    cal_parms <- read_lines(project_path%//%"cal_parms.cal", skip = 3) %>%
      strsplit(., "\\s+") %>%
      map(., ~ .x[1]) %>%
      unlist(.)
    in_cal_parms <- parameter$definition$parameter %in% cal_parms

    if(any(!in_cal_parms)){
      stop("Parameters"%&&%
           paste(parameter$definition$par_name[!in_cal_parms], collapse = ", ")%&&%
           "not defined in 'cal_parms.cal'")
    }
  } else {
    stop("The file 'cal_parms.cal is missing in SWAT+ project!")
  }
}
