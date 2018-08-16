#' Define output variables
#'
#' Define which output variables (from the parallel SWAT execution of the
#' function \code{\link{run_swat}}) should be saved and returned after finishing
#' the model runs. Note that, the variable \code{output} in
#' \code{\link{run_swat}} must be defined via this function.
#'
#' @param ... Variables that will be provided as output after the execution
#'   of \code{run_SWAT()}. The variables are defined with the helper
#'   function \code{variable()}. See the examples for possible application.
#'
#' @return Returns a tibble of the output variables to provide to the
#'   parameter \code{output} of the function \code{run_SWAT()}
#' @importFrom purrr map_df
#' @importFrom tibble add_column
#' @importFrom dplyr mutate select
#' @export
#'
#' @examples
#' # Writing the discharge for the RCH unsits 1 to 5
#' # and the nitrogen load and ET for the RCH or SUB unit 5:
#'   out_tbl <- define_output(flow = out_var(output_file = "rch",
#'                                            variable_name = "FLOW_OUT",
#'                                            spatial_unit = 1:5),
#'                            no3  = out_var(output_file = "rch",
#'                                            variable_name = "NO3_OUT",
#'                                            spatial_unit = 5),
#'                            ptot = out_var(output_file = "rch",
#'                                            variable_name = "PTOT",
#'                                            spatial_unit = 5),
#'                            et_a = out_var(output_file = "sub",
#'                                            variable_name = "ET",
#'                                            spatial_unit = 5))
#'
#'
# define_output <- function(...){
#   var_input <- c(as.list(environment()), list(...))
#   var_nrow <- lapply(var_input, nrow) %>% unlist
#
#   map_df(var_input, rbind) %>%
#     add_column(label = rep(names(var_input), var_nrow), .before = "file") %>%
#     mutate(label = paste0(label, label_ind)) %>%
#     select(-label_ind)
# }

#' Helper function for the \code{\link{run_swat}}) output variable definition with
#' \code{\link{define_output}}
#'
#' Each variable provided with the function \code{\link{define_output}}should be
#' defined using the helper function \code{\link{variable}}.
#'
#' @param output_file The SWAT output output_file where the output variable is located (so
#'   far only .sub and .rch are supported!)
#' @param variable_name The name of the variable as given in the header of the
#'   respective output output_file (without unsits!)
#' @param spatial_unit The 'RCH' or 'SUB unit for which the output should be
#'   written.
#' @param expr Caution! Advanced setting if the output variable should be
#'   modified or aggregated before writing the output. To do so a piped
#'   workflow must be provided (see examples).
#' @param add_date Logical parameter whether a date should be provided for
#'   the output or not. E.g. can be set FALSE if temporal aggregation was
#'   applied to the output variable and the date would have no meaning for
#'   the variable.
#' @return The function should only be used as a helper function for
#'   \code{define_output()}
#' @importFrom tibble tibble
#' @importFrom pasta %.% %&&%
#' @export
#' @examples
#' # Writing the discharge for the RCH unsits 1 to 5
#' # and the nitrogen load and ET for the RCH or SUB unit 5:
#' out_tbl <- define_output(flow = out_var(output_file   = "rch",
#'                                         variable_name = "FLOW_OUT",
#'                                         spatial_unit  = 1:5),
#'                          no3  = out_var(output_file   = "rch",
#'                                         variable_name = "NO3_OUT",
#'                                         spatial_unit  = 5),
#'                          ptot = out_var(output_file   = "rch",
#'                                         variable_name = "PTOT",
#'                                         spatial_unit  = 5),
#'                          et_a = out_var(output_file   = "sub",
#'                                         variable_name = "ET",
#'                                         spatial_unit  = 5))
#'
#'
#' # Writing the dicharge for RCH 16,28, and 31 and the sum of discharge for
#' # all subbasins and time steps (as defined in sum_expr):
#'
#' sum_expr <- "dplyr::filter(MON <= 366) %>%
#'                dplyr::select(FLOW_OUT) %>%
#'                dplyr::summarise(FLOW_SUM = sum(FLOW_OUT))"
#'
#' out_tbl <- define_output(flow = out_var(output_file   = "rch",
#'                                         variable_name = "FLOW_OUT",
#'                                         spatial_unit  = c(16,28,31)),
#'                          flow_sum = out_var(output_file = "rch",
#'                                             expression  = sum_expr,
#'                                             add_date    = FALSE))
#'
define_output <- function(file, variable = NULL, unit = NULL,
                          expression = NULL){
  if(!is.null(expression) &
     (!is.null(unit) | !is.null(variable))){
    stop("If 'expression' is given, 'spatial_unit'"%&&%
         "and 'variable_name' must be NULL.")
  }

  if(is.null(expression) &
     (is.null(unit) | is.null(variable))){
    stop("A selection by 'variable' requires a 'unit' value.")
  }

  if(!is.null(unit) &
     !(typeof(unit) %in% c("double", "integer"))){
    stop("'unit' must be a number or a vector of numbers.")
  }

  if(!is.null(variable) & (length(variable) > 1)){
    stop("Only one variable is allowed in 'define_output()'.")
  }

  output_file <- output_file %>%
    gsub("\\.", "", .) %>%
    gsub("output", "", .) %>%
    paste0("output.",.)


  if(nchar(output_file) == 3) {
    output_file <- "output"%.%output_file
  }

  if(!(output_file %in% c("output.rch", "output.sub"))){
    stop("Wrong input for 'output_file' in out_var()."%&&%
         "So far only '.rch' and '.sub' supported")
  }

  variable_name <- variable_name %>%
    gsub("Mg/l|mg/L|mg/kg|kg/ha|kg/h|t/ha|mic/L|\\(mm\\)|kg|cms|tons|mg|mm|km2| ", "", .)

  if(is.null(expression)){
    expression <- paste0("dplyr::filter(.[[2]] == ",  spatial_unit, ") %>% ",
                         "dplyr::filter(MON <= 366) %>% ",
                         "dplyr::select(", variable_name, ") %>% ",
                         "magrittr::set_colnames('run'%_%sprintf('%06d',run_i))")
  }

  if(length(spatial_unit) > 1){
    label_ind <- paste0("_",spatial_unit)
  } else {
    label_ind <- ""
  }

  tibble(file = output_file,
         expr = expression,
         label_ind = label_ind,
         date = add_date)
}
