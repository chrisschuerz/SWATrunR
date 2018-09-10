library(tidyverse)
library(pasta)


translate_parameter_constraints <- function(par) {

  has_par_name <- map_lgl(par, ~ grepl("\\:\\:", .x))
  has_change   <- map_lgl(par, ~ grepl("change", .x))

  if(any(!has_change)) stop("Type of change must be provided for all parameters!")

  bool_op <- "\\=|\\=\\=|\\!\\=|\\<|\\<\\=|\\>|\\>\\="

  model_par <- tibble(par_name  = gsub("\\:\\:.*", "", par) %>% trimws(.),
                      parameter = gsub(".*\\:\\:|\\|.*","", par) %>% trimws(.)) %>%
    mutate(file_name  = gsub(".*\\.","", parameter),
           parameter  = gsub("\\..*","", parameter),
           par_name   = ifelse(has_par_name, par_name, parameter),
           change     = gsub(".*change|\\|.*", "", par) %>%
             gsub(bool_op, "",.) %>%
             trimws(.) %>%
             substr(., 1, 3))

  is_correct_change <- model_par$change %in% c("rel", "abs", "rep")
  if(any(!is_correct_change)) {
    stop("Wrong input for change type. Must be either 'rel', 'abs', or 'rep'.")
  }

  is_correct_file <- model_par$file_name %in%
    c("pnd", "rte", "sub", "swq", "hru", "gw", "sdr", "sep", "bsn", "wwq",
      "res", "ops", "sol", "mgt", "chm")

  if(any(!is_correct_file)) {
    stop(paste(model_par$file_name[!is_correct_file], collapse = ", ")%&&%
         "files are no valid file type!")
  }

  unique_par <- table(model_par$par_name)
  if(any(unique_par > 1)) {
    stop("Duplicated parameter names found! Define individual names with:\n"%&%
         "par_name::parameter|...")
  }

  filter_expr <- par %>%
    strsplit(., "\\|") %>%
    map(., ~ .x[2:length(.x)] %>% .[!grepl("change",.)])

  filter_var <- map(filter_expr, ~ gsub("["%&%bool_op%&%"].*", "", .x)) %>%
    map(., ~ trimws(.x)) %>%
    map(., ~ tolower(.x))
  filter_val <- map(filter_expr, ~ gsub(".*["%&%bool_op%&%"]", "", .x)) %>%
    map(., ~ trimws(.x)) %>%
    map(., ~ concat_values(.x))
  filter_op  <- map(filter_expr, ~ gsub("[^"%&%bool_op%&%"]", "", .x)) %>%
    map(., ~ trimws(.x)) %>%
    map(., ~ gsub("\\=", "\\=\\=", .x )) %>%
    map(., ~ substr(.x, 1, 2))

  expressions <- pmap(list(filter_var, filter_val, filter_op), build_expr) %>%
    map_df(., as_tibble)

  model_par <- map2(filter_op, filter_val, paste) %>%
    map2(.,filter_var, ~ as.list(c(NA,.x)) %>% set_names(., c("idx",.y)) %>% as_tibble(.)) %>%
    bind_rows(.) %>%
    select(-idx) %>%
    select(one_of("subbasin", "hru", "luse", "soil", "slope"), everything()) %>%
    bind_cols(model_par, ., expressions)

  return(model_par)
}



concat_values <- function(x){
  x[!grepl("c\\((.*?)\\)",x) & grepl("\\,", x)] <-
    "c("%&%x[!grepl("c\\((.*?)\\)",x) & grepl("\\,", x)]%&%")"
  return(x)
}

build_expr <- function(var, val, op) {
  is_file_var <- var %in% c("subbasin", "hru", "luse", "soil", "slope")

    file_filter <- pmap_chr(list(var[is_file_var], val[is_file_var], op[is_file_var]),
           build_filter) %>%
    paste(., collapse = " %>% ")
  spec_filter <- pmap_chr(list(var[!is_file_var], val[!is_file_var], op[!is_file_var]),
                          build_filter) %>%
    paste(., collapse = " %>% ")

  return(list(file_expression = file_filter, spec_expression = spec_filter))
}

build_filter <- function(var, val, op) {
  if(grepl("\\,|\\:", val)){
    if(op == "==") {
      expr <- "filter(.,"%&&%var%&&%"%in%"%&&%val%&%")"
    } else if (op == "!=") {
      expr <- "filter(., !("%&%var%&&%"%in%"%&&%val%&%"))"
    } else {
      stop("Operator"%&&%op%&&%"not valid together with multiple filter values.")
    }
  } else {
    expr <- "filter(.,"%&&%var%&&%op%&&%val%&%")"
  }
  return(expr)
}

