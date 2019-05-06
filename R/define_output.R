#' Define SWAT output variables
#'
#' Define the SWAT output variables that should be extracted after the SWAT
#' model execution and be returned to R. It is required to use this function to
#' pass the desired outputs with the variable \code{output} in the function calls
#' \code{\link{run_swat2012}} and \code{\link{run_swatplus}}. See
#' the examples how to use the \code{output} definition together with
#' \code{\link{run_swat2012}} or \code{\link{run_swatplus}}. Further, more comprehensive
#' examples are provided on the package's 'Get Started' page in the section
#' '\href{https://chrisschuerz.github.io/SWATplusR/articles/SWATplusR.html#first-swat-model-runs}{First SWAT model runs}.
#'
#' @param file Character string. The SWAT output file to read.
#' (Valid inputs are \code{'rch'}, \code{'sub'}, \code{'hru'}, and \code{'sed'} for
#'   the respective SWAT2012 output files 'output.rch', 'output.sub',
#'   output.hru', and 'output.sed'. For the respective SWAT+ output files see
#'   the available options in the 'print.prt' file of your SWAT+ project).
#' @param variable Character string. Output variable that is extracted from the
#'   respective SWAT output file defined with \code{file}. For a correct
#'   definition of SWAT2012 output variables please use the
#'   variable documented in the
#'   \href{https://swat.tamu.edu/media/69395/ch32_output.pdf}{SWAT Output Data
#'   Documentation}. For SWAT+ output variables please use the header names from
#'   the respective output table, \strong{without the units!} Optionally, the
#'   column number of a variable in the respective output file can be provided.
#'   \strong{CAUTION:} spaces (e.g. in P TOT) must be replaced with underscores
#'   (P_TOT).
#' @param unit Numeric vector. The spatial unit (e.g. the reach, subbasin, or
#'   HRU) defined by the columns 'RCH', 'SUB', 'HRU' in the respective SWAT2012
#'   output file or the 'unit' column in the SWAT+ output file for which the
#'   outputs should be extracted.
#' @param expression As an alternative to \code{variable} and \code{unit} an
#'   expression can be defined as a string to perform an individual extraction of outputs.
#'   This is still experimental and is not necessary in most cases!
#' @importFrom tibble tibble
#' @export
#' @examples
#' # A single variable can be defined as follows (e.g. "FLOW_OUT" for
#' # the reaches 1 and 5):
#'
#' out_flow <- define_output(file = "rch",
#'                           variable = "FLOW_OUT",
#'                           unit = c(1,5))
#' # In this case the the variable name of the returned output is then
#' # the same as defined with 'variable', here "FLOW_OUT"
#'
#' # If a custom variable name is preferred for the returned output,
#' # the output must be defined as named list:
#'
#' out_flow <- list(discharge = define_output(file = "rch",
#'                                            variable = "FLOW_OUT",
#'                                            unit = c(1,5)))
#'
#'
#' # Define the discharge for the RCH units 1 to 5 and the
#' # nitrate-nitrogen load and ET for the unit 5:
#'
#' out_def <- list(flow = define_output(file = "rch",
#'                                      variable = "FLOW_OUT",
#'                                      unit = 1:5),
#'                 no3  = define_output(file = "rch",
#'                                      variable = "NO3_OUT",
#'                                      unit = 5),
#'                 et_a = define_output(file = "sub",
#'                                      variable = "ET",
#'                                      unit = 5))
#'
#'
#' # Define output with an expression:
#' # E.g. directly extract P_TOT concentrations for reach 5 from
#' # daily simulations:
#'
#' expr <- paste0("dplyr::filter(., RCH == 5) %>% ",
#'                "dplyr::filter(., MON %in% 1:12) %>% ",
#'                "dplyr::select(., FLOW_OUT, P_TOT) %>% ",
#'                "dplyr::mutate(., P_CONC = P_TOT/FLOW_OUT/86.4) %>% ",
#'                "dplyr::select(., P_CONC)")
#'
#' out_def <- list(p_conc = define_output(file ="rch", expression = expr))
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


  file_check <- file %>%
    substr(., (nchar(.) - 2), nchar(.))

  vers <- ifelse(file_check %in% c("rch", "sub", "hru", "sed"), "2012", "plus")

  if(vers == "2012") {
    file <- "output"%.%file_check
  }

  variable <- variable %>%
    remove_units_2012(.) %>%
    remove_units_plus(.)


  if((vers == "2012") & is.null(expression)){
    expression <- paste0("dplyr::filter(.[[2]] == ", unit, ") %>% ",
                         "dplyr::filter(!is.na(MON)) %>% ",
                         "dplyr::filter(MON > (quantile(MON, probs = 0.75, type = 3) - 300)) %>% ",
                         "dplyr::filter(MON < (quantile(MON, probs = 0.75, type = 3) + 200)) %>% ",
                         "dplyr::select( ", variable, " )")
  } else {
    expression <- paste0("dplyr::filter(unit == ", unit, ") %>% ",
                         "dplyr::select( ", variable, " )")
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

#' Check output if is a data.frame and convert in case to named list
#'
#' @param output Defined output
#'
#' @keywords internal
#'
check_output <- function(output) {
  if(is.data.frame(output)) {
    var_name <- output$expr[1] %>%
      strsplit(., " %>%") %>%
      unlist(.) %>%
      .[[length(.)]] %>%
      gsub("dplyr\\:\\:select\\(", "", .) %>%
      gsub("\\)", "", .) %>% trimws(., "both")

    output <- list(output)
    names(output) <- var_name
  }
  return(output)
}
