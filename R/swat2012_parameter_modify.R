parameter <- c("sft::SFTMP.bsn|change = rep" = 0.75,
               "snocmx::SNOCOVMX.bsn|change = rep" = 0.5,
               "gw_del::GW_DELAY.gw|change = rel|" = 0.2,
               "bio_init::BIO_INIT.mgt|change = rel|" = 0.2,
               "cnop_till::CNOP.mgt|change = rel|mgt_op = 6" = 0.2)
parameter <- SWATplusR:::format_parameter(parameter)
project_path <- "/home/christoph/Documents/projects/SWATtapiR/swat_test"
file_meta <- SWATplusR:::read_file_meta(project_path, parameter$parameter_constrain)
swat_parameter <- SWATplusR:::read_swat2012_files(project_path,file_meta)

thread_parameter <- swat_parameter

i_run <- 1

for (i_par in 1:ncol(parameter$values)) {
  file_i <- parameter$parameter_constrain[i_par, ]$file_name

  if(file_i == "mgt") {
    swat_par_i <- parameter$parameter_constrain$parameter[i_par]
    gen_mgt_par <- c("IGRO", "PHU_PLT", "BIOMIX", "CN2", "USLE_P", "BIO_MIN",
                     "FILTERW", "IURBAN", "URBLU", "IRRSC", "IRRNO", "FLOWMIN",
                     "DIVMAX", "FLOWFR", "DDRAIN", "TDRAIN", "GDRAIN")
    if(swat_par_i %in% gen_mgt_par){
      thread_parameter <-
        modify_par(parameter, thread_parameter, file_meta, i_par, i_run)
    } else {
      thread_parameter <-
        modify_mgt_par(parameter, thread_parameter, file_meta, i_par, i_run)
    }
  } else {
    thread_parameter <-
      modify_par(parameter, thread_parameter, file_meta, i_par, i_run)
  }
}

modify_par <- function(parameter, model_parameter, file_meta,
                            i_par, i_run) {
  par_i <- parameter$parameter_constrain[i_par, ]
  par_up_i <- parameter$values[[par_i$par_name]][i_run]
  file_code_i <- par_i$file_expression %>%
    evaluate_expression(file_meta, .) %>%
    .[["file_code"]]

  idx_i <- model_parameter[[par_i$file_name]][["value"]] %>%
    filter(file_code %in% file_code_i) %>%
    filter_spec_expr(., par_i$spec_expression) %>%
    .[["idx"]]

  model_parameter[[par_i$file_name]][["value"]][[par_i$parameter]] <-
    model_parameter[[par_i$file_name]][["value"]][[par_i$parameter]] %>%
    update_par(., par_up_i, par_i$change, idx_i)

  return(model_parameter)
}

modify_mgt_par <- function() {
  mgt_lookup <- tribble(
    ~par,           ~mgt_op,     ~mgt_i,
    "PLANT_ID",     1,           "MGT1",
    "CURYR_MAT",    1,           "MGT3",
    "HEAT_UNITS",   1,           "MGT4",
    "LAI_INIT",     1,           "MGT5",
    "BIO_INIT",     1,           "MGT6",
    "HI_TARG",      1,           "MGT7",
    "BIO_TARG",     1,           "MGT8",
    "CNOP_TILL",    1,           "MGT9",
    "IRR_AMT",      2,           "MGT4",
    "FERT_ID",      3,           "MGT1",
    "FRT_KG",       3,           "MGT4",
    "FRT_SURFACE",  3,           "MGT5",
    "PEST_ID",      4,           "MGT1",
    "PST_KG",       4,           "MGT4",
    "CNOP_HRVST",   5,           "MGT4",
    "TILL_ID",      6,           "MGT1",
    "CNOP_TILL",    6,           "MGT4",
    "HARVEFF",      7,           "MGT4",
    "HI_OVR",       7,           "MGT5",
    "GRZ_DAYS",     9,           "MGT1",
    "MANURE_ID",    9,           "MGT2",
    "BIO_EAT",      9,           "MGT4",
    "BIO_TRMP",     9,           "MGT5",
    "MANURE_KG",    9,           "MGT6",
    "WSTRS_ID",     10,          "MGT1",
    "AUTO_WSTRS",   10,          "MGT4",
    "AFERT_ID",     11,          "MGT1",
    "AUTO_NSTRS",   11,          "MGT4",
    "AUTO_NAPP",    11,          "MGT5",
    "AUTO_NYR",     11,          "MGT6",
    "AUTO_EFF",     11,          "MGT7",
    "AFRT_SURFACE", 11,          "MGT8",
    "SWEEPEFF",     12,          "MGT4",
    "FR_CURB",      12,          "MGT5",
    "IMP_TRIG",     13,          "MGT1",
    "FERT_DAYS",    14,          "MGT1",
    "CFRT_ID",      14,          "MGT2",
    "IFRT_FREQ",    14,          "MGT3",
    "CFRT_KG",      14,          "MGT4")


}

update_par <- function(par, par_up, change, to_chg){
  par[to_chg] <- case_when(
    change == "rep" ~ par_up,
    change == "rel" ~ par[to_chg] * (1 + par_up),
    change == "abs" ~ par[to_chg] + par_up
    )
  return(par)
}

is_in_file_codes <- function(code_chg, code_file) {
  code_file %in% code_chg
}

filter_spec_expr <- function(table, expr) {
  if(nchar(expr) > 0) {
    table <- evaluate_expression(table, expr)
  }
  return(table)
}
