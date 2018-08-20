check_parameter <- function(parameter, abs_swat_val) {
  if(is.null(abs_swat_val)) {
    abs_swat_file <- readLines(system.file("extdata", "Absolute_SWAT_Values.txt",
                                           package = "SWATplusR"))
  } else {
    abs_swat_file <- readLines(abs_swat_val, warn = FALSE)
  }
  file_pos <- grep("//+ .", abs_swat_file)
}
