# Functions for reading SWAT outputs

#' Read SWAT output files
#'
#' @param output Output defined to read from the SWAT model results
#' @param thread_path Path to respective thread where SWAT was executed
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map map2 map_chr set_names
#' @importFrom readr fwf_positions read_fwf
#' @keywords internal
#'
read_swat2012_output <- function(output, thread_path) {
  ## Get unique output files defined in output
  output_file <- map_chr(output,  ~ unique(.x$file)) %>% unique(.)

  ## Find the first position of table i neach file
  frst_pos <- find_first_line(output_file, thread_path)
  ## Get the variable positions in all output files
  fwf_pos     <- map2(output_file, frst_pos, ~ get_fwf_positions(.x, thread_path, .y))
  ## Get the column header for all output files
  file_header <- pmap(list(output_file, frst_pos),
                      function(out, frst, thread_path) {
                        get_file_header(out, frst, thread_path)},
                      thread_path)

  ## Read all output files, assign column names and assign output file names
  out_tables <- pmap(list(output_file, fwf_pos, frst_pos),
                     function(out, fwf, frst, thread_path) {
                       read_fwf(file = thread_path%//%out,
                                col_positions = fwf_positions(fwf[[1]], fwf[[2]]),
                                skip = frst, guess_max = 3)}, thread_path) %>%
    map2(., file_header, ~set_names(.x, .y)) %>%
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

#' Read the column names for the SWAT output files
#'
#' @param output Output defined to read from the SWAT model results
#' @param fwf_pos Fixed width positions for the variables in the output files
#' @param thread_path Path to respective thread where SWAT was executed
#'
#' @importFrom dplyr %>%
#' @importFrom readr read_lines
#' @keywords internal
#'
get_file_header <- function(output_i, tbl_pos, thread_path) {
  header <- read_lines(file = thread_path%//%output_i,
                       skip = tbl_pos - 1, n_max = 1) %>%
    split_by_units(.)
  header[1] <- "FILE"
  return(header)
}

#' Derive the fixed with positions of the columns in the SWAT output files
#'
#' @param output Output defined to read from the SWAT model results
#' @param thread_path Path to respective thread where SWAT was executed
#'
#' @importFrom dplyr %>%
#' @importFrom readr read_lines
#' @importFrom stringr str_sub
#' @keywords internal
#'
get_fwf_positions <- function(output_i, thread_path, tbl_pos) {
  header_line <- read_lines(file = thread_path%//%output_i,
                            skip = tbl_pos - 1, n_max = 1)
  first_line <- read_lines(file = thread_path%//%output_i,
                           skip = tbl_pos, n_max = 1) %>%
    str_sub(., 1, nchar(header_line))
  # Start pos must be tweaked due to untidy spacing in output tables -> ugly tweak with 39 :()
  start_pos <- c(1,find_first_space(header_line)[find_first_space(header_line) < 39],
                   find_first_space(first_line)) %>%
    unique(.) %>%
    sort(.)
  duplicates <- which(diff(start_pos) == 1)
  start_pos <- start_pos[-duplicates]
  end_pos <- c((start_pos[2:length(start_pos)] - 1), nchar(header_line))
  return(list(start_pos, end_pos))
}

#' Helper function to find the position of the fist line of the table in a file
#'
#' @param out_file The output files to be read
#' @param thread_path Path to respective thread where SWAT was executed
#'
#' @importFrom purrr map map_int
#' @importFrom readr read_lines
#' @keywords internal
#'
find_first_line <- function(out_file, thread_path) {
  file_head <- map(out_file, ~ read_lines(thread_path%//%.x, n_max = 50))
  head_line <- map_int(file_head, ~ which(grepl("GIS", .x) & grepl("MON", .x)))
  return(head_line)
}

#' Helper function to find the fist position of white spaces in a text string
#'
#' @param string text string that contains words seperated by white spaces
#'
#' @importFrom dplyr %>%
#' @keywords internal
#'
find_first_space <- function(string) {
  single_char <- strsplit(string, "") %>% unlist(.)
  space_pos <- which(single_char == " ")
  space_pos_diff <- c(0,diff(space_pos))
  space_pos[space_pos_diff != 1]
}

#' Helper function to convert file.cio entries to numerics
#'
#' @param cio_entry Line from file.cio
#' @importFrom stringr str_sub
#'
#' @keywords internal
#'
cio_to_numeric <- function(cio_entry) {
  cio_entry %>% str_sub(., 1, 16) %>% as.numeric(.)
}

#' Remove the units from variable names in output files of SWAT2012 simulations
#'
#' @param col_nm Character vector with column names
#'
#' @keywords internal
#'
remove_units_2012 <- function(col_nm) {
  unit <- "Mg\\/l$|mg\\/L$|mg\\/kg$|mg|kg\\/ha$|kg\\/h$|kg|t\\/ha$|mic\\/L$|\\(mm\\)$|kg$|cms$|tons$|ton$|mg$|mg\\/$|mm$|km2$|_tha$|_kgha$|\\_m$|\\_kgN\\/ha$|\\_kgP\\/ha$|\\_m\\^3$|ha\\-m$|_k$|mgps$|  |"
  col_nm <- gsub(unit, "", col_nm) %>%
    gsub("\\_$", "", .)
  return(col_nm)
}

split_by_units <- function(header) {
  unit <- "Mg\\/l|mg\\/L|mg\\/kg|mg|kg\\/ha|kg\\/h|kg|t\\/ha|mic\\/L|\\(mm\\)|kg|cms|tons|ton|mg|mg\\/|mm|km2|_tha|_kgha|\\_m|\\_kgN\\/ha|\\_kgP\\/ha|\\_m\\^3|ha\\-m|_k|mgps|degC|degc|ct|  "
  strsplit(header, unit) %>%
    unlist(.) %>%
    trimws() %>%
    gsub(" ", "_", .) %>%
    .[nchar(.) > 0]
}
