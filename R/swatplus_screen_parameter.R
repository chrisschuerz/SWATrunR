# SCREEN
screen <- function(project_path, parameter, calibration_var, tolerance = 0.05,
                   output = NULL, start_date = NULL, end_date = NULL,
                   output_interval = NULL, years_skip = NULL,
                   run_path = NULL, n_thread = NULL) {

  if(is.null(output)) {
    output <- define_output(file = "basin_wb", variable = "prec", unit = 1)
    output_interval <- "aa"
    return_output <- FALSE
  } else {
    return_output <- TRUE
  }

  n_thread <- min(max(nrow(parameter$values),1),
                  max(n_thread,1),
                  detectCores())

  parameter_bound <- parameter

  lhs_samp <- optimumLHS(n_thread, ncol(parameter_bound))
  par_samp <- map2_df(parameter_bound, as_tibble(lhs_samp),
                      ~ ((max(.x) - min(.x))*.y))

  sim <- run_swatplus(project_path = project_path, output = output,
                      start_date = start_date, end_date = end_date,
                      output_interval = output_interval, years_skip = years_skip,
                      keep_folder = TRUE, quiet = FALSE,
                      return_output = return_output, soft_calibration = TRUE)


}



