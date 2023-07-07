#-------------------------------------------------------------------------------
# Functions for output variable extraction

#' Prepare run info for the simulation experiment
#'
#' @param sim_results List of simulation results from the SWAT model runs
#' @param model_setup List of model configurations
#' @param output Table of defined output variables
#' @param project_path Path to the SWAT project folder
#' @param run_path Path to the SWAT project run folder
#' @param t0 Start date time stamp
#'
#' @importFrom dplyr bind_cols %>%
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
initialize_run_info <- function(model_setup, output, project_path, run_path, t0) {
  run_info <- list()

  run_info$simulation_log <- tibble(run_started  = t0,
                                    run_finished = NA,
                                    run_time = NA,
                                    project_path = project_path,
                                    run_path = run_path)

  run_info$simulation_period <-   model_setup[c("start_date", "end_date", "years_skip",
                                                "start_date_print", "output_interval")] %>%
    .[!is.na(names(.))] %>%
    bind_cols(.)

  run_info$output_definition <- output
  run_info$output_definition$unit <- map_chr(run_info$output_definition$unit, group_values)

  return(run_info)
}

#' Prepare run info for the simulation experiment
#'
#' @param sim_results List of simulation results from the SWAT model runs
#' @param output Table of defined output variables
#' @param run_index Vector of run IDs
#' @param project_path Path to the SWAT project folder
#' @param t1 End date time stamp
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate as.period interval
#' @importFrom purrr map_lgl
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
add_run_info <- function(run_info, sim_result, run_index, t1) {
  is_result <- map_lgl(sim_result, is.list)
  run_info$simulation_log$run_index_finished <- list(run_index[is_result])
  run_info$simulation_log$run_index_error    <- list(run_index[!is_result])

  t_diff <- interval(run_info$simulation_log$run_started, t1) %>%
    round(.) %>%
    as.period(.)

  run_info$simulation_log$run_finished <- t1
  run_info$simulation_log$run_time <- t_diff

  return(run_info)
}

#' Prepare error report in case of failed simulations
#'
#' @param sim_results List of simulation results from the SWAT model runs
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map_chr map_lgl set_names
#' @importFrom stringr str_remove
#' @importFrom tibble add_column enframe
#' @keywords internal
#'
prepare_error_report <- function(sim_result) {
  is_result <- map_lgl(sim_result, is.list)

  if(!all(is_result)) {
    error_report <- sim_result[!is_result] %>%
      enframe() %>%
      set_names(c("run", "message")) %>%
      add_column(. ,error = map_chr(.$message,
                                    ~.x[which(.x == 'Error:') + 1]),
                 .after = "run") %>%
      add_column(. ,idx = as.numeric(str_remove(.$run, "run_")), .before = 1)


  } else {
    error_report <- NULL
  }

  return(error_report)
}

#' Tidy up and rearrange simulation results before returning them
#'
#' @param sim_results List of simulation results from the SWAT model runs
#'
#' @importFrom dplyr bind_cols %>%
#' @importFrom purrr list_flatten map map2 map_lgl set_names list_transpose
#' @keywords internal
#'
tidy_simulations <- function(sim_result) {
  is_result <- map_lgl(sim_result, is.list)
  sim_result <- sim_result[is_result]

  if(length(sim_result) > 0) {
    add_cols <- map(sim_result[[1]], ~ extract_non_var_cols(.x))
    var_cols <- map(sim_result, ~ map(.x, ~ remove_non_var_cols(.x)))

    var_add_assgn <- map2(var_cols[[1]],
                          1:length(var_cols[[1]]),
                          ~ rep(.y, ncol(.x))) %>%
      list_c(.)

    var_cols <- var_cols %>%
      map(., ~ map(.x, ~ map(.x, ~.x))) %>%
      map(., ~ list_flatten(.x)) %>%
      list_transpose(., simplify = FALSE) %>%
      map(., ~ bind_cols(.x))

    add_cols <- add_cols[var_add_assgn]

    sim_result <- map2(add_cols, var_cols, ~bind_cols(.x, .y)) %>%
      set_names(names(var_cols))


  } else {
    sim_result <- NULL
  }

  return(sim_result)
}

#' Extract date, plant_name, p columns from the respective simulation outputs
#'
#' @param tbl Table of simulation results for one SWAT output file
#'
#' @keywords internal
#'
extract_non_var_cols <- function(tbl) {
  if (names(tbl)[1] == 'date') {
    col_extr <- tbl[1]
  } else if(all(names(tbl)[c(1,2)] == c('year', 'plant_name'))) {
    col_extr <- tbl[1:2]
  } else if (names(tbl)[1] == 'p' & all(tbl[[1]][1:3] == c(0, 0.1, 0.5))) {
    col_extr <- tbl[1]
  } else {
    col_extr <- NULL
  }
  return(col_extr)
}

#' Extract variable columns from the respective simulation outputs
#'
#' @param tbl Table of simulation results for one SWAT output file
#'
#' @keywords internal
#'
remove_non_var_cols <- function(tbl) {
  if (names(tbl)[1] == 'date') {
    tbl <- tbl[2:ncol(tbl)]
  } else if(all(names(tbl)[c(1,2)] == c('year', 'plant_name'))) {
    tbl <- tbl[3:ncol(tbl)]
  } else if (names(tbl)[1] == 'p' & all(tbl[[1]][1:3] == c(0, 0.1, 0.5))) {
    tbl <- tbl[2:ncol(tbl)]
  }
  return(tbl)
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
