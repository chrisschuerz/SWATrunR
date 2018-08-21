#' Write the Model.in file required for rewriting paramters with SWAT_Edit.exe'
#'
#' @param parameter Named parameter vector or parameter table
#' @param thread_path Path to respective thread where parameters are rewritten
#' @param i_run Index of current run to select the current parameter set
#'
#' @importFrom dplyr %>%
#' @importFrom pasta %//%
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
#' @importFrom dplyr %>%
#' @importFrom pasta %//%
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
#' @importFrom dplyr %>%
#' @importFrom pasta %//%
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
#' @importFrom dplyr %>%
#' @importFrom pasta %//%
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
#' @importFrom dplyr %>%
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

#' Extract the variables from the model outputs as defined in 'output'
#'
#' @param output Output defined to read from the SWAT model results
#' @param model_output Output files read from the respective thread
#'
#' @importFrom dplyr bind_cols bind_rows mutate %>%
#' @importFrom purrr map map2 pmap set_names
#' @keywords internal
#'
extract_output <- function(output, model_output) {
  output %>%
    map2(., names(.), ~mutate(.x, label_ind = paste0(.y, label_ind))) %>%
    bind_rows(.) %>%
    map(., ~.x) %>%
    pmap(., function(file, expr, label_ind, mod_out){
      mod_out[[file]] %>%
        evaluate_expression(., expr) %>%
        set_names(., label_ind)
    }, mod_out = model_output) %>%
    bind_cols(.)
}

#' Evaluate the expression defined for a variable in 'output'
#'
#' @param output_table Output defined to read from the SWAT model results
#' @param expression Expression to be applied to extract variable from
#'   output_table
#'
#' @importFrom dplyr %>%
#' @keywords internal
#'

evaluate_expression <- function(out_table, expression){
  paste("out_table", expression, sep = " %>% ") %>%
    parse(text = .) %>%
    eval(.)
}
