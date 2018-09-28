# SCREEN
screen <- function(project_path, parameter, wb_fraction, tolerance = 0.05,
                   ee_iter_max = 3,
                   output = NULL, start_date = NULL, end_date = NULL,
                   output_interval = NULL, years_skip = NULL,
                   run_path = NULL, n_thread = NULL) {

  ## Run internal SWAT+ soft calibration
  ## Initialize all soft calibration files
  ### ls_regions.cal: file defining the targeted fractions for the water balance
  ls_cal_val <- tibble(NAME = "all_lum",
                       SRR = wb_fraction[["srr"]],
                       LFR = wb_fraction[["lfr"]],
                       PCR = wb_fraction[["pcr"]],
                       ETR = wb_fraction[["etr"]],
                       TFR = wb_fraction[["tfr"]],
                       SED = 0, ORGN = 0, ORGP = 0, NO3 = 0, SOLP = 0)
  ls_cal <- c("ls_cal.reg",
              "1",
              "NAME    NLUM",
              "basin    1",
              paste(names(ls_cal_val), collapse = " "),
              paste(ls_cal_val, collapse = " "))

  ### codes.cal: indication what to implement in soft calibration (fixed so far)
  codes_cal <- c("codes.cal",
                 "HYD_HRU HYD_HRUL PLT SED NUT CHSED CHNUT RES",
                 paste(c("  y ", rep("  n ", 7)), collapse = ""))

  ## Build .model_run for soft_calibration run
  if(is.null(run_path)){
    cal_path <- project_path%//%".model_run"
  } else {
    cal_path <- run_path%//%".model_run"
  }

  unlink(cal_path, recursive = TRUE)
  build_model_run(project_path, cal_path, n_thread = 1, quiet = T)

  soft_cal_setup <- setup_swatplus(project_path = project_path,
                                   parameter = NULL,
                                   output = list(p = define_output(file = "basin_wb",
                                                          variable = "prec",
                                                          unit = 1)),
                                   start_date, end_date,
                                   output_interval = "aa", years_skip,
                                   soft_cal = TRUE)

  ## Write soft calibration files to project folder
  write_lines(ls_cal, project_path%//%"ls_regions.cal")
  write_lines(codes_cal, project_path%//%"codes.cal")




  if(is.null(output)) {
    output <- define_output(file = "basin_wb", variable = "prec", unit = 1)
    output_interval <- "aa"
    return_output <- FALSE
  } else {
    return_output <- TRUE
  }

  cal_names <- names(calibration_var)
  cal_output <- map(cal_names, ~ define_output(file = "waterbal_aa_bsn.txt",
                                               variable = .x,
                                               unit = 1)) %>%
    set_names(cal_names)
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
                        parameter = par_samp,
                        start_date = start_date, end_date = end_date,
                        output_interval = output_interval,
                        years_skip = years_skip, run_path = run_path,
                        n_thread = n_thread, keep_folder = TRUE, quiet = FALSE,
                        return_output = return_output, soft_calibration = TRUE)


    threads <- dir(cal_path) %>% .[grepl("thread_", .)]

    cal_sim <- map(threads, ~ read_swatplus_output(cal_output,
                                                   cal_path%//%.x)) %>%
      map(., ~ extract_output(cal_output, .x)) %>%
      bind_rows()

  }


}

#' Find threshold between informative and non-informative parameters based on
#' their sensitivities
#'
#' @param sens_value Numeric vector of parameter sensitivities
#'
#' @importFrom dplyr %>%
#'
#' @keywords internal
#'
find_sensitivity_threshold <- function(sens_value) {
  sens_norm <- (sens_value - min(sens_value))/
    (max(sens_value) - min(sens_value)) %>%
    sort(.)
  x <- seq(0, 1, length.out = length(sens_norm))

  logi_opt <- optim(par = c(0,1,1,0), sse, x = x, y = sens_norm, fun = logi)

  x_est <- seq(0,1,length.out = 100*length(sens_norm))
  k <- logi_curv(x_est, res$par)
  max_curv <- which.max(k)
  thres_norm <- logi(x_est, logi_opt$par)[max_curv]
  sens_thres <- (max(sens_value) - min(sens_value)) * thres_norm + min(sens_value)

  return(sens_thres)
}

#' Logistic regression function with offset
#'
#' @param x Numeric vector of x values
#' @param par Numeric parameter set, providing (1) offset, (2) Curve's max
#'   point, (3) steepness, and (4) x value of sigmoid mid-point
#'
#' @keywords internal
#'
logi <- function(x, par) {par[1] + par[2]/(1 + exp(-par[3]*(x - par[4])))}

#' First derivative of the logistic regression function
#'
#' @param x Numeric vector of x values
#' @param par Numeric parameter set, providing (1) offset, (2) Curve's max
#'   point, (3) steepness, and (4) x value of sigmoid mid-point
#'
#' @keywords internal
#'
logi_drv1 <- function(x, par) {
  par[2]*par[3]/ (2*cosh(par[3]*(x - par[4])) + 1)
}

#' Second derivative of the logistic regression function
#'
#' @param x Numeric vector of x values
#' @param par Numeric parameter set, providing (1) offset, (2) Curve's max
#'   point, (3) steepness, and (4) x value of sigmoid mid-point
#'
#' @keywords internal
#'
logi_drv2 <- function(x, par) {
  -par[3]^2*par[2]*sinh(par[3]*(x - par[4]))/
    (2*cosh(par[3]*(x - par[4])) + 1)^2
}

#' Curvature of the logistic regression function
#'
#' @param x Numeric vector of x values
#' @param par Numeric parameter set, providing (1) offset, (2) Curve's max
#'   point, (3) steepness, and (4) x value of sigmoid mid-point
#'
#' @keywords internal
#'
logi_curv <- function(x, par) {
  l_reg_drv2(x, par)/(1 + l_reg_drv1(x, par))^(3/2)
}

#' Sum of squared errors
#'
#' @param x Numeric vector of x values
#' @param y Numeric vector of 'observed' y values
#' @param par Numeric vector of parameter set
#' @param fun Model function to evaluate
#'
#' @keywords internal
#'
sse <- function(x, y, par, fun) {
  sum((y - fun(x, par))^2)
}



