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
read_output <- function(output, thread_path) {
  ## Get unique output files defined in output
  output_file <- map_chr(output,  ~ unique(.x$file)) %>% unique(.)
  ## Get the variable positions in all output files
  fwf_pos     <- map(output_file, ~ get_fwf_positions(.x, thread_path))
  ## Get the column header for all output files
  file_header <- map2(output_file, fwf_pos,
                      ~ get_file_header(.x, .y, thread_path))

  ## Read all output files, assign column names and assign output file names
  out_tables <- map2(output_file, fwf_pos,
                     ~ read_fwf(file = thread_path%//%.x,
                                col_positions = fwf_positions(.y[[1]], .y[[2]]),
                                skip = 9, guess_max = 3)) %>%
    map2(., file_header, ~set_names(.x, .y)) %>%
    set_names(., output_file)

  tables_nrow <- map(out_tables, ~nrow(.x)) %>% unlist(.)
  if(any(tables_nrow == 0)){
    stop("\nOne of the SWAT runs was not successful!\n"%&&%
         "A reason can be the defined model parameters.\n"%&&%
         "Please checkif any change in the model parametrization"%&&%
         "Caused any parameter to be out of bounds!")
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
#' @importFrom pasta %//%
#' @importFrom readr fwf_positions read_fwf
#' @keywords internal
#'
get_file_header <- function(output_i, fwf_pos, thread_path) {
  header <- read_fwf(file = thread_path%//%output_i, skip = 8, n_max = 1,
                     col_positions = fwf_positions(fwf_pos[[1]], fwf_pos[[2]])) %>%
    gsub("Mg/l|mg/L|mg/kg|kg/ha|kg/h|t/ha|mic/L|\\(mm\\)|kg|cms|tons|mg|mm|km2|", "", .) %>%
    gsub(" ", "_",.)
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
  # Start pos must be tweaked due to untidy spacing in output tables -> ugly tweak with 39 :()
  start_pos <- c(1,find_first_space(header_line)[find_first_space(header_line) < 39],
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

#' Tidy up simulation results before returning them
#'
#' @param sim_results Extracted simulation results from the SWAT model runs
#' @param parameter Provided parameter set
#' @param file_cio Modified file.cio
#' @param save_parameter Logical. If TRUE parameters are saved in outputs
#' @param add_date Logical. If TRUE Dates are added to the simulation results
#'
#' @importFrom dplyr bind_cols %>%
#' @importFrom pasta %&%
#' @importFrom purrr map set_names transpose
#' @importFrom tibble as_tibble
#' @keywords internal
#'
tidy_results <- function(sim_result, parameter, file_cio, save_parameter,
                         add_date) {
  if(length(sim_result) == 1) {
      sim_result <- sim_result[[1]]
  } else {
    n_digit <- length(sim_result) %>% as.character(.) %>% nchar(.)
    sim_result <- sim_result %>%
      set_names(., "run"%_%sprintf("%0"%&%n_digit%&%"d", 1:length(sim_result))) %>%
      transpose(.) %>%
      map(., ~ as_tibble(.x))
  }

  if(add_date) {
    sim_date <- read_date(file_cio)

    if(is.data.frame(sim_result)){
      sim_result <- bind_cols(sim_date, sim_result)
    } else {
      sim_result <- map(sim_result, ~ bind_cols(sim_date, .x))
    }
  }

  if(save_parameter & !is.null(parameter)) {
    sim_result <- list(parameter  = parameter,
                       simulation = sim_result)
  }

  return(sim_result)
}

#' Read file.cio parameters and convert the required ones to a date sequence
#'
#' @param file_cio Modified file.cio
#'
#' @importFrom dplyr case_when %>%
#' @importFrom lubridate as_date
#' @importFrom pasta %_%
#' @importFrom tibble tibble
#' @keywords internal
#'
read_date <- function(file_cio) {
  n_year     <- cio_to_numeric(file_cio[8])
  years_skip <- cio_to_numeric(file_cio[60])
  start_year <- cio_to_numeric(file_cio[9]) + years_skip
  end_year   <- start_year + n_year - years_skip - 1
  start_jdn  <- cio_to_numeric(file_cio[10])
  end_jdn    <- cio_to_numeric(file_cio[11])
  time_int   <- cio_to_numeric(file_cio[59])

  start_date <- as_date(start_jdn%_%start_year, format = "%j_%Y", tz = "UTC")
  end_date  <- as_date(end_jdn%_%end_year, format = "%j_%Y", tz = "UTC")

  by_int <- case_when(time_int == 0 ~ "month",
                      time_int == 1 ~ "day",
                      time_int == 2 ~ "year")

  tibble(date = seq(start_date, end_date, by = by_int))
}

#' Helper function to convert file.cio entries to numerics
#'
#' @param cio_entry Line from file.cio
#'
#' @keywords internal
#'
cio_to_numeric <- function(cio_entry) {
  cio_entry %>% substr(., 1, 16) %>% as.numeric(.)
}
