#' Checks if all provided parameters exist in 'Absolute_SWAT_Values'
#'
#' @param parameter Named parameter vector or parameter table
#' @param abs_swat_val (optional) path to custom 'Absolute_SWAT_Value' file
#'
#' @importFrom dplyr %>%
#' @importFrom pasta %&% %&&%
#' @importFrom purrr map2 map_chr
#' @keywords internal
#'

check_parameter <- function(parameter, abs_swat_val) {
  if(is.null(abs_swat_val)) {
    abs_swat_file <- readLines(system.file("extdata", "Absolute_SWAT_Values.txt",
                                           package = "SWATplusR"))
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
