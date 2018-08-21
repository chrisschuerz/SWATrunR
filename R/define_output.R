#' Define output variables
#'
#' Define which output variables (from the parallel SWAT execution of the
#' function \code{\link{run_swat}}) should be saved and returned after finishing
#' the model runs. Note that, the variable \code{output} in
#' \code{\link{run_swat}} must be defined via this function.


#' Helper function for the \code{\link{run_swat}}) output variable definition with
#' \code{\link{define_output}}
#'
#' Each variable provided with the function \code{\link{define_output}}should be
#' defined using the helper function \code{\link{variable}}.
#'
#' @param file The SWAT output output_file where the output variable is located (so
#'   far only .sub and .rch are supported!)
#' @param variable The name of the variable as given in the header of the
#'   respective output output_file (without unsits!)
#' @param unit The 'RCH' or 'SUB unit for which the output should be
#'   written.
#' @param expr Caution! Advanced setting if the output variable should be
#'   modified or aggregated before writing the output. To do so a piped
#'   workflow must be provided (see examples).
#' @return The function should only be used as a helper function for
#'   \code{define_output()}
#' @importFrom tibble tibble
#' @importFrom pasta %.% %&&%
#' @export
#' @examples
#' # Writing the discharge for the RCH unsits 1 to 5
#' # and the nitrogen load and ET for the RCH or SUB unit 5:
#' out_tbl <- define_output(flow = out_var(output_file   = "rch",
#'                                         variable = "FLOW_OUT",
#'                                         spatial_unit  = 1:5),
#'                          no3  = out_var(output_file   = "rch",
#'                                         variable = "NO3_OUT",
#'                                         spatial_unit  = 5),
#'                          ptot = out_var(output_file   = "rch",
#'                                         variable = "PTOT",
#'                                         spatial_unit  = 5),
#'                          et_a = out_var(output_file   = "sub",
#'                                         variable = "ET",
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
#'                                         variable = "FLOW_OUT",
#'                                         spatial_unit  = c(16,28,31)),
#'                          flow_sum = out_var(output_file = "rch",
#'                                             expression  = sum_expr,
#'                                             add_date    = FALSE))
#'
define_output <- function(file, variable = NULL, unit = NULL,
                          expression = NULL){
  if(!is.null(expression) &
     (!is.null(unit) | !is.null(variable))){
    stop("If 'expression' is given, 'unit' and 'variable' must be NULL.")
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

  file <- file %>%
    substr(., (nchar(.) - 2), nchar(.)) %>%
    paste0("output.",.)

  if(!(file %in% c("output.rch", "output.sub", "output.hru", "output.sed"))){
    stop("Wrong input for 'file'."%&&%
         "Supported output files are: '.rch', '.sub', '.hru', and '.sed'.")
  }

  variable <- variable %>%
    gsub("Mg/l|mg/L|mg/kg|kg/ha|kg/h|t/ha|mic/L|\\(mm\\)|kg|cms|tons|mg|mm|km2| ", "", .)

  if(is.null(expression)){
    expression <- paste0("dplyr::filter(.[[2]] == ",  unit, ") %>% ",
                         "dplyr::filter(MON > (quantile(MON, probs = 0.75, type = 3) - 300)) %>% ",
                         "dplyr::filter(MON < (quantile(MON, probs = 0.75, type = 3) + 200)) %>% ",
                         "dplyr::filter(MON <= 366) %>% ",
                         "dplyr::select(", variable, ")")
  }

  if(length(unit) > 1){
    label_ind <- paste0("_",unit)
  } else {
    label_ind <- ""
  }

  return(tibble(file      = file,
                expr      = expression,
                label_ind = label_ind))
}
