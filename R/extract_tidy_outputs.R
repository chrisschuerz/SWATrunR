#-------------------------------------------------------------------------------
# Functions for output variable extraction

#' Extract the variables from the model outputs as defined in 'output'
#'
#' @param output Output defined to read from the SWAT model results
#' @param model_output Output files read from the respective thread
#'
#' @importFrom dplyr bind_cols %>%
#' @importFrom purrr map2
#' @keywords internal
#'
extract_output <- function(output, model_output) {
  output %>%
    transpose(.) %>%
    map(., ~extract_out_i(.x, model_output)) %>%
    bind_cols()
}

#' Extract the variable i from the simulation outputs
#'
#' @param out_i The ith output defined to read from the SWAT model results
#' @param out_i_name The name of the ith output variable
#' @param mod_out Output files read from the respective thread
#'
#' @importFrom dplyr bind_cols %>%
#' @importFrom purrr map map2 set_names
#' @keywords internal
#'
extract_out_i <- function(out_i, mod_out) {
  if(length(out_i$unit) > 1) {
    out_i$name <- paste(out_i$name, out_i$unit, sep = "_")
  }
  var <- out_i$expr %>%
    paste0('table %>% ', .) %>%
    evaluate_expression(mod_out[[out_i$file]], .)
  map(out_i$unit, ~var[var[,1] == .x, 2]) %>%
    map2(., out_i$name, ~set_names(.x, .y)) %>%
    bind_cols()
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
    if(!is_result) {
      error_report <- sim_result
      sim_result <- NULL
    } else {
      error_report <- NULL
    }
  } else {
    if(!is.null(parameter)) {
      n_digit <- get_digit(parameter$values)
    } else {
      n_digit <- length(sim_result) %>% as.character(.) %>% nchar(.)
    }

    sim_result <- set_names(sim_result, "run"%_%sprintf("%0"%&%n_digit%&%"d", run))

    is_result <- map_lgl(sim_result, is.list)

    if(!all(is_result)) {
      error_report <- sim_result[!is_result] %>%
        enframe() %>%
        set_names(c("run", "message"))
      error_report <- error_report %>%
        add_column(. ,error = map_chr(.$message, ~.x[which(.x == 'Error:') + 1]), .after = "run") %>%
        add_column(. ,idx = as.numeric(str_remove(.$run, "run_")), .before = 1)
    } else {
      error_report <- NULL
    }
    if(!all(!is_result)) {
      sim_result <- sim_result[is_result] %>%
        transpose(.) %>%
        map(., ~ as_tibble(.x))
    } else {
      sim_result <- NULL
    }
  }

  if(add_date & !is.null(sim_result)) {
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

  if(!is.null(error_report)) {
    if(add_parameter & !is.null(parameter$value)) {
      sim_result$error_report <- error_report
    } else if (is.null(sim_result) & is.character(error_report)) {
      sim_result <- error_report
    } else {
      sim_result <- list(simulation = sim_result,
                         error_report = error_report)
    }
  }

  return(sim_result)
}

#' Create date vector from the date info in the model setup of a SWAT2012 project
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
get_date_vector_2012 <- function(model_setup) {
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
