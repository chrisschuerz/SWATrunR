# Functions for reading SWAT+ outputs

#' Read SWAT+ output files
#'
#' @param output Output defined to read from the SWAT model results
#' @param thread_path Path to respective thread where SWAT was executed
#'
#' @importFrom dplyr %>%
#' @importFrom pasta %//%
#' @importFrom purrr map map2 map_chr set_names
#' @importFrom readr fwf_positions read_fwf
#' @keywords internal
#'
read_swatplus_output <- function(output, thread_path) {
  ## Get unique output files defined in output
  output_file <- map_chr(output,  ~ unique(.x$file)) %>% unique(.)

  col_names <- map(output_file, ~ read_table(thread_path%//%.x, skip = 1,
                                             n_max = 1, col_names = F)) %>%
    map(., ~ unlist(.x) %>% unname(.))

  is_col_duplicate <- map(col_names, function(x){
                                       duplicates <- table(x) %>%
                                         .[. > 1] %>%
                                         names(.)

                                       x %in% duplicates})

  ## Read all output files, assign column names and assign output file names
  out_tables <- map(output_file,~ read_table(file = thread_path%//%.x,
                                             col_names = FALSE, skip = 3, )) %>%
    map2(., col_names, ~ .x[, 1:length(.y)]) %>%
    map2(., col_names, ~ set_names(.x, .y)) %>%
    map2(., is_col_duplicate, ~ .x[!.y]) %>%
    set_names(., output_file)

  tables_nrow <- map(out_tables, ~nrow(.x)) %>% unlist(.)
  if(any(tables_nrow == 0)){
    stop("\nOne of the SWAT runs was not successful!\n"%&&%
         "A reason can be the defined model parameters.\n"%&&%
         "Please check if any change in the model parametrization"%&&%
         "caused any parameter to be out of bounds!")
  }

  return(out_tables)
}

#' Translate the output file settings defined according to print.prt to the
#' actual output file names
#'
#' @param output List of output variables defined with \code{define_output}
#' @param output_interval the time interval for writing the simulated outputs
#'
#' @importFrom dplyr %>% case_when
#' @importFrom tibble tribble
#' @importFrom purrr map
#' @keywords internal
#'
translate_outfile_names <- function(output, output_interval) {
  output_interval <- substr(output_interval, 1,1) %>% tolower(.)
  output_interval <-
    case_when(output_interval == "d" ~ "_day",
              output_interval == "m" ~ "_mon",
              output_interval == "y" ~ "_yr",
              output_interval == "a" ~ "_aa")
  out_files <- map(output, ~ paste0(.x$file[[1]], output_interval, ".txt"))
  map2(output, out_files, function(out, out_file){
                            out$file <- out_file
                            return(out)})
}

