#' Checks if all provided parameters exist in 'Absolute_SWAT_Values'
#'
#' @param parameter Named parameter vector or parameter table
#' @param abs_swat_val (optional) path to custom 'Absolute_SWAT_Value' file
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map map2 map_chr
#' @keywords internal
#'

check_parameter <- function(parameter, abs_swat_val) {
  ## Check for parameter duplicates
  if(length(names(parameter)) != length(unique(names(parameter)))) {
    stop("Duplicates in provided parameters were found!")
  }

  ## Read 'Absolute_SWAT_Values' file from custom file if provided or internal
  if(is.null(abs_swat_val)) {
    abs_swat_file <- readLines(system.file("extdata", "Absolute_SWAT_Values.txt",
                                           package = "SWATplusR"), warn = FALSE)
  } else {
    abs_swat_file <- readLines(abs_swat_val, warn = FALSE)
  }

  file_pos <- grep("//+[[:space:]]+\\.", abs_swat_file)

  file_label <- abs_swat_file[file_pos] %>%
    gsub("^.*\\.", "",.) %>%
    map2(.,diff(c(file_pos, (length(abs_swat_file) + 1))), ~rep(.x,.y)) %>%
    unlist(.)

  all_par <- abs_swat_file %>%
    strsplit(., "[[:space:]]") %>%
    map(., ~.x[1]) %>%
    unlist(.) %>%
    paste(.,file_label, sep = ".")

  par_names <- names(parameter) %>%
    strsplit(., "__") %>%
    map_chr(., ~.x[2]) %>%
    gsub("\\{.*?\\}", "",.) %>%
    gsub("\\(.*?\\)", "",.)

  par_exist <- par_names %in% all_par

  if(any(!par_exist)) {
    stop("The following parameters were not found in the 'Absolute_SWAT_Value' file:"%&&%
         paste(par_names[!par_exist], collapse = ", ")%&%
         "\n\nTo fix the problem use a custom 'Absolute_SWAT_Value' file that"%&&%
         "contains these parameters and provide the path of this file through"%&&%
         "the parameter 'abs_swat_val'.")
  }
}

#' Checks the provided run_index vector in accordance with provided parameters
#'
#' @param run_index Numeric vector to subset the provided parameter sets
#' @param parameter Named parameter vector or parameter table
#'
#' @importFrom dplyr %>%
#' @keywords internal
#'

check_run_index <- function(run_index, parameter) {
  #Sort and check run_indices

  if(!is.numeric(run_index)) stop("'run_index' must be a numeric vector!")
  if(max(run_index) > max(nrow(parameter),1)) {
    stop("'run_index' includes larger values than the available numer of parameter sets.")
  }
  if(any(run_index <= 0)) stop("No negative 'run_index' allowed.")

  return(run_index %>% sort(.) %>% unique(.))
}

