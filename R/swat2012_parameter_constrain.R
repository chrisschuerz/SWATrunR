library(tidyverse)
library(pasta)


translate_parameter_constraints <- function(parameter) {
  bool_op <- "\\=|\\=\\=|\\!\\=|\\<|\\<\\=|\\>|\\>\\="

  model_par <- tibble(par_name  = gsub("\\:\\:.*", "", a) %>% trimws(.),
                      parameter = gsub(".*\\:\\:|\\|.*","", a) %>% trimws(.)) %>%
    mutate(file_name  = gsub(".*\\.","", parameter),
           parameter = gsub("\\..*","", parameter),
           change    = gsub(".*change|\\|.*", "", a) %>%
             gsub(bool_op, "",.) %>%
             trimws(.))

  filter_expr <- a %>%
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
    map2(.,filter_var, ~ as.list(.x) %>% set_names(., .y) %>% as_tibble(.)) %>%
    bind_rows(.) %>%
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

