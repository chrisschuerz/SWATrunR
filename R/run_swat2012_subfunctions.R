#' Write the Model.in file required for rewriting paramters with SWAT_Edit.exe'
#'
#' @param parameter Named parameter vector or parameter table
#' @param thread_path Path to respective thread where parameters are rewritten
#' @param i_run Index of current run to select the current parameter set
#'
#' @keywords internal
#'

write_model_in <- function(parameter, thread_path, i_run){
  if(!is.null(dim(parameter))){
    parameter <- parameter[i_run,]
  }
  data.frame(parameter = names(parameter),
             value = as.numeric(parameter),
             stringsAsFactors = FALSE) %>%
    write.table(., file = thread_path%//%"model.in",
                quote = FALSE, row.names = FALSE, col.names = FALSE)
}


#-------------------------------------------------------------------------------
# Functions for reading SWAT outputs

#' Read SWAT output files
#'
#' @param output Output defined to read from the SWAT model results
#' @param thread_path Path to respective thread where SWAT was executed
#'
#' @importFrom purrr map map2 map_chr set_names
#' @importFrom readr fwf_positions read_fwf
#' @keywords internal
#'

read_output <- function(ouput, thread_path) {
  ## Get unique output files defined in output
  output_file <- map_chr(output,  ~ unique(.x$file)) %>% unique(.)
  ## Get the variable positions in all output files
  fwf_pos     <- map(output_file, ~ get_fwf_positions(.x, thread_path))
  ## Get the column header for all output files
  file_header <- map2(output_file, fwf_pos,
                      ~ get_file_header(.x, .y, thread_path))

  ## Read all output files, assign column names and assign output file names
  map2(output_file, fwf_pos, ~ read_fwf(file = thread_path%//%.x,
                                        col_positions = fwf_positions(.y[[1]], .y[[2]]),
                                        skip = 9, guess_max = 3)) %>%
    map2(., file_header, ~set_names(.x, .y)) %>%
    set_names(., output_file)
}

#' Read the column names for the SWAT output files
#'
#' @param output Output defined to read from the SWAT model results
#' @param fwf_pos Fixed width positions for the variables in the output files
#' @param thread_path Path to respective thread where SWAT was executed
#'
#' @importFrom readr fwf_positions read_fwf
#' @keywords internal
#'

get_file_header <- function(output_i, fwf_pos, thread_path) {
  header <- read_fwf(file = thread_path%//%output_i, skip = 8, n_max = 1,
                     col_positions = fwf_positions(fwf_pos[[1]], fwf_pos[[2]])) %>%
    gsub("Mg/l|mg/L|mg/kg|kg/ha|kg/h|t/ha|mic/L|\\(mm\\)|kg|cms|tons|mg|mm|km2| ", "", .)
  header[1] <- "FILE"
  return(header)
}

#' Derive the fixed with positions of the columns in the SWAT output files
#'
#' @param output Output defined to read from the SWAT model results
#' @param thread_path Path to respective thread where SWAT was executed
#'
#' @importFrom readr read_lines
#' @keywords internal
#'

get_fwf_positions <- function(output_i, thread_path) {
  header_line <- read_lines(file = thread_path%//%output_i,
                            skip = 8, n_max = 1)
  first_line <- read_lines(file = thread_path%//%output_i,
                           skip = 9, n_max = 1) %>%
    substr(., 1, nchar(header_line))
  start_pos <- c(1,find_first_space(header_line),
                   find_first_space(first_line)) %>%
    unique(.) %>%
    sort(.)
  end_pos <- c((start_pos[2:length(start_pos)] - 1), nchar(header_line))
  return(list(start_pos, end_pos))
}

#' Helper function to find the fist position of white spaces in a text string
#'
#' @param string text string that contains words seperated by white spaces
#'
#' @keywords internal
#'

find_first_space <- function(string) {
  single_char <- strsplit(string, "") %>% unlist(.)
  space_pos <- which(single_char == " ")
  space_pos_diff <- c(0,diff(space_pos))
  space_pos[space_pos_diff != 1]
}


#-------------------------------------------------------------------------------
# Functions for output variable extraction

## Extract variables from SWAT output file according to "output"
extract_var <- function(out_var_i, out_file, run_i) {
  out_file <- out_file[[out_var_i[2]]]
  extr_tbl <- eval_expr(out_file, out_var_i[3], run_i)
  tbl_colnames <- colnames(extr_tbl)

  name_idx <- strsplit(tbl_colnames, "_") %>%
    lapply(.,as.numeric) %>%
    lapply(., na.exclude) %>%
    unlist

  if(length(name_idx) == 0 | any(name_idx != run_i)){
    colnames(extr_tbl) <- tbl_colnames%_%sprintf('%06d',run_i)
  }
  return(extr_tbl)
}

## Evaluate expression provided as string in output. Defines mutates on out_tbl
eval_expr <- function(out_tbl, expr, run_i){
  paste("out_tbl", expr, sep = " %>% ") %>%
    parse(text = .) %>%
    eval(.)
}
