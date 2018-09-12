
thread_parameter <- swat_parameter

parameter$parameter_constrain


update_par <- function(par, par_up, change, to_chg){
  par[to_chg] <- case_when(
    change == "rep" ~ par_up,
    change == "rel" ~ par[to_chg] * (1 + par_up),
    change == "abs" ~ par[to_chg] + par_up
    )
  return(par)
}

i_run <- 1

for (i_par in 1:ncol(parameter$values)) {
  file_i     <- parameter$parameter_constrain$file_name[i_par]
  par_i      <- parameter$parameter_constrain$par_name[i_par]
  swat_par_i <- parameter$parameter_constrain$parameter[i_par]
  change_i   <- parameter$parameter_constrain$change[i_par]
  to_chg_i   <- parameter$parameter_constrain$file_expression[i_par] %>%
    evaluate_expression(file_meta, .) %>%
    .[["file_code"]] %>%
    is_in_file_codes(., thread_parameter[[file_i]][["value"]][["file_code"]])
  par_up_i   <- parameter$values[[par_i]][i_run]

  thread_parameter[[file_i]][["value"]][[swat_par_i]] <-
    thread_parameter[[file_i]][["value"]][[swat_par_i]] %>%
      update_par(., par_up_i, change_i, to_chg_i)
}

is_in_file_codes <- function(code_chg, code_file) {
  code_file %in% code_chg
}
