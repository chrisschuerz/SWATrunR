library(tidyverse)
library(pasta)

a <- c("cnop_till::CNOP.mgt|change == rel|op = c(6)|luse != URBM,FRST,FRSD|subbasin >= 16",
       "cnop_hrvst::CNOP.mgt|change=rel|op=5|subbasin = 1:15",
       "cnop_plant::CNOP.mgt|change=rel|op=5|subbasin=1:15")
bool_op <- "\\=|\\=\\=|\\!\\=|\\<|\\<\\=|\\>|\\>\\="

model_par <- tibble(par_name  = gsub("\\:\\:.*", "", a) %>% trimws(.),
                    parameter = gsub(".*\\:\\:|\\|.*","", a) %>% trimws(.)) %>%
  mutate(file_sfx  = gsub(".*\\.","", parameter),
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

expressions <- pmap_chr(list(filter_var, filter_val, filter_op), build_expr)

concat_values <- function(x){
  x[!grepl("c\\((.*?)\\)",x) & grepl("\\,", x)] <-
    "c("%&%x[!grepl("c\\((.*?)\\)",x) & grepl("\\,", x)]%&%")"
  return(x)
}

build_expr <- function(var, val, op) {
  pmap_chr(list(var, val, op), build_filter) %>%
    paste(., collapse = " %>% ")
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

