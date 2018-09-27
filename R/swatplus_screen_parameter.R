# SCREEN
screen <- function(project_path, parameter, calibration_var,
                   tolerance = 0.05, sc_min = 1, output = NULL,
                   start_date = NULL, end_date = NULL,
                   output_interval = NULL, years_skip = NULL,
                   run_path = NULL, n_thread = NULL) {

  if(is.null(output)) {
    output <- define_output(file = "basin_wb", variable = "prec", unit = 1)
    output_interval <- "aa"
    return_output <- FALSE
  } else {
    return_output <- TRUE
  }

  ### Defeine the outputs for soft calibration here also with define output to
  ### then use the read output function from the R package

  n_thread <- min(max(n_thread,1),
                  detectCores())

  parameter_bound <- parameter

  lhs_samp <- optimumLHS(n_thread, ncol(parameter_bound))
  par_samp <- map2_df(parameter_bound, as_tibble(lhs_samp),
                      ~ ((max(.x) - min(.x))*.y))

  while (sc_found <= sc_min) {
    sim <- run_swatplus(project_path = project_path, output = output,
                        start_date = start_date, end_date = end_date,
                        output_interval = output_interval,
                        years_skip = years_skip, run_path = run_path,
                        n_thread = n_thread, keep_folder = TRUE, quiet = FALSE,
                        return_output = return_output, soft_calibration = TRUE)


  }


}



