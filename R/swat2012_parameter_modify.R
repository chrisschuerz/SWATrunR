parameter <- c("sft::SFTMP.bsn|change = rep" = 0.75,
               "snocmx::SNOCOVMX.bsn|change = rep" = 0.5,
               "no3_sol::SOL_NO3.chm|change = rel|" = 0.2,
               "p_sol::SOL_SOLP.chm|change = rel|layer < 3" = 0.2)
parameter <- SWATplusR:::format_parameter(parameter)
project_path <- "/home/christoph/Documents/projects/SWATtapiR/swat_test"
file_meta <- SWATplusR:::read_file_meta(project_path, parameter$parameter_constrain)
swat_parameter <- SWATplusR:::read_swat2012_files(project_path,file_meta)

thread_parameter <- swat_parameter

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
  file_i <- parameter$parameter_constrain[i_par, ]$file_name

  case_when(
    file_i %in% c("pnd", "rte", "sub", "swq", "hru", "gw",
                  "sdr", "sep", "bsn", "wwq", "res", "ops") ~ {
    thread_parameter <-
    modify_list_par(parameter, thread_parameter, file_meta, i_par, i_run)
    },
    file_i == "mgt" ~ {thread_parameter <- thread_parameter}
    )
}

modify_list_par <- function(parameter, model_parameter, file_meta,
                            i_par, i_run) {
  par_i <- parameter$parameter_constrain[i_par, ]
  par_up_i <- parameter$values[[par_i$par_name]][i_run]

  to_chg_i   <- par_i$file_expression %>%
    evaluate_expression(file_meta, .) %>%
    .[["file_code"]] %>%
    is_in_file_codes(.,
      model_parameter[[par_i$file_name]][["value"]][["file_code"]])

  model_parameter[[par_i$file_name]][["value"]][[par_i$parameter]] <-
    model_parameter[[par_i$file_name]][["value"]][[par_i$parameter]] %>%
    update_par(., par_up_i, par_i$change, to_chg_i)

  return(model_parameter)
}

modify_chm_par <- function() {

}

modify_sol_par <- function() {

}

modify_mgt_par <- function() {

}
is_in_file_codes <- function(code_chg, code_file) {
  code_file %in% code_chg
}
