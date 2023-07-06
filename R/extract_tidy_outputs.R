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
#' @importFrom stringr str_extract
#' @importFrom tibble add_column as_tibble enframe
#' @keywords internal
#'
tidy_results <- function(sim_result, parameter, add_parameter,
                         add_date, run) {
  n_digit <- length(sim_result) %>% as.character(.) %>% nchar(.)
  sim_result <- set_names(sim_result, "run"%_%sprintf("%0"%&%n_digit%&%"d", run))

  is_result <- map_lgl(sim_result, is.list)
  if(!all(is_result)) {
    error_report <- sim_result[!is_result] %>%
      enframe() %>%
      set_names(c("run", "message")) %>%
      add_column(. ,error = map_chr(.$message, ~.x[which(.x == 'Error:') + 1]), .after = "run") %>%
      add_column(. ,idx = as.numeric(str_remove(.$run, "run_")), .before = 1)

    sim_result <- sim_result[is_result]
  } else {
    error_report <- NULL
  }

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

  output_list <- list()

  if(add_parameter & !is.null(parameter$value)) {
    output_list$parameter <- parameter
  }

  output_list$simulation <- sim_result

  if(!is.null(error_report)) {
    output_list$error_report <- error_report
  }

  return(output_list)
}


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

  # if(length(sim_result) == 1) {
  #   sim_result <- sim_result[[1]]
  #   is_result <- is.data.frame(sim_result)
  #   if(!is_result) {
  #     error_report <- sim_result
  #     sim_result <- NULL
  #   } else {
  #     error_report <- NULL
  #   }
  # } else {
  #   if(!is.null(parameter)) {
  #     n_digit <- get_digit(parameter$values)
  #   } else {
  #     n_digit <- length(sim_result) %>% as.character(.) %>% nchar(.)
  #   }


    # is_run_name <- max(nchar(str_extract(names(sim_result[1]), 'run_')), 0) > 0







  # if(add_date & !is.null(sim_result)) {
  #   sim_date <- date
  #   if(is.data.frame(sim_result)){
  #     sim_result <- bind_cols(sim_date, sim_result)
  #   } else {
  #     sim_result <- map(sim_result, ~ bind_cols(sim_date, .x))
  #   }
  # }




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
