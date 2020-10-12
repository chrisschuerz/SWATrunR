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
#' @importFrom purrr map map_chr map_lgl set_names transpose
#' @importFrom stringr str_remove
#' @importFrom tibble add_column as_tibble enframe
#' @keywords internal
#'
tidy_results <- function(sim_result, parameter, date, add_parameter,
                         add_date, run) {
  if(length(sim_result) == 1) {
    sim_result <- sim_result[[1]]
    is_result <- is.data.frame(sim_result)
  } else {
    if(!is.null(parameter)) {
      n_digit <- nrow(parameter$values) %>% as.character(.) %>% nchar(.)
    } else {
      n_digit <- length(sim_result) %>% as.character(.) %>% nchar(.)
    }

    sim_result <- set_names(sim_result, "run"%_%sprintf("%0"%&%n_digit%&%"d", run))

    is_result <- map_lgl(sim_result, is.list)

    if(!all(is_result)) {
      err_report <- sim_result[!is_result] %>%
        enframe() %>%
        set_names(c("run", "message"))
      err_report <- err_report %>%
        add_column(. ,error = map_chr(.$message, ~.x[1]), .after = "run") %>%
        add_column(. ,idx = as.numeric(str_remove(.$run, "run_")), .before = 1)
    }

    sim_result <- sim_result[is_result] %>%
      transpose(.) %>%
      map(., ~ as_tibble(.x))
  }

  if(add_date & all(is_result)) {
    sim_date <- date

    if(is.data.frame(sim_result)){
      sim_result <- bind_cols(sim_date, sim_result)
    } else {
      sim_result <- map(sim_result, ~ bind_cols(sim_date, .x))
    }
  }

  if(add_parameter & !is.null(parameter$value)) {
    sim_result <- list(parameter  = parameter,
                       simulation = sim_result)
  }

  if(!all(is_result) & !is.data.frame(sim_result) & !is.character(sim_result)) {
    if(is.null(sim_result$simulation) & is.null(sim_result$simulation)) {
      sim_result <- list(simulation = sim_result,
                         error_report = err_report)
    } else {
      sim_result$error_report <- err_report
    }
  }

  return(sim_result)
}

#' Create date vector from the date info in the model setup
#'
#' @param model_setup Model setup defined by run_swat input parameters and/of
#'   the SWAT model input files
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate floor_date year
#' @importFrom tibble tibble
#' @importFrom stringr str_sub
#' @keywords internal
#'
get_date_vector <- function(model_setup) {
  int <- model_setup$output_interval %>% str_sub(., 1, 1)
  y_skip <-  model_setup$years_skip
  sd  <- (model_setup$start_date + years(y_skip)) %>% floor_date(., unit = "y")
  ed  <- model_setup$end_date

  if(int %in% c("d", "m", "y")) {
    date <- seq(sd, ed, by = int) %>% floor_date(., unit = int)
  } else {
    date <- paste(year(sd), year(ed), sep = " - ")
  }

  date <- tibble(date = date)
  return(date)
}
