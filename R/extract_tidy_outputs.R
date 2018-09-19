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

#' Tidy up simulation results before returning them
#'
#' @param sim_results Extracted simulation results from the SWAT model runs
#' @param parameter Provided parameter set
#' @param file_cio Modified file.cio
#' @param add_parameter Logical. If TRUE parameters are saved in outputs
#' @param add_date Logical. If TRUE Dates are added to the simulation results
#'
#' @importFrom dplyr bind_cols %>%
#' @importFrom pasta %&%
#' @importFrom purrr map set_names transpose
#' @importFrom tibble as_tibble
#' @keywords internal
#'
tidy_results <- function(sim_result, parameter, date, add_parameter,
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
    sim_date <- date

    if(is.data.frame(sim_result)){
      sim_result <- bind_cols(sim_date, sim_result)
    } else {
      sim_result <- map(sim_result, ~ bind_cols(sim_date, .x))
    }
  }

  if(add_parameter & !is.null(parameter)) {
    sim_result <- list(parameter  = parameter,
                       simulation = sim_result)
  }

  return(sim_result)
}
