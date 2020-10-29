#' Translate the parameter inputs and the set constraints into lookup table
#'
#' @param par Character string vector providing parameters and constraints
#'
#' @importFrom dplyr %>% bind_cols bind_rows everything mutate one_of select
#' @importFrom purrr map map2 map_lgl pmap set_names
#' @importFrom tibble as_tibble tibble
#' @importFrom stringr str_extract str_split str_sub
#' @keywords internal
#'
translate_parameter_constraints <- function(par) {

  has_par_name <- map_lgl(par, ~ grepl("\\:\\:", .x))
  has_change   <- map_lgl(par, ~ grepl("change", .x))

  if(any(!has_change)) stop("Type of change must be provided for all parameters!")

  bool_op <- "\\=\\=|\\!\\=|\\<\\=|\\>\\=|\\=|\\<|\\>|\\%in\\%"

  model_par <- tibble(par_name  = gsub("\\:\\:.*", "", par) %>% trimws(.),
                      parameter = gsub(".*\\:\\:|\\|.*","", par) %>% trimws(.)) %>%
    mutate(file_name  = gsub(".*\\.","", parameter),
           parameter  = gsub("\\..*","", parameter),
           par_name   = ifelse(has_par_name, par_name, parameter),
           change     = gsub(".*change|\\|.*", "", par) %>%
             gsub(bool_op, "",.) %>%
             trimws(.) %>%
             str_sub(., 1, 6))

  is_correct_change <- model_par$change %in% c("relchg", "pctchg", "abschg", "absval")
  if(any(!is_correct_change)) {
    stop("Wrong input for change type. Must be either"%&&%
         "'relchg', 'pctchg', 'abschg', or 'absval'.")
  }

  # Here future!: add reading file_name frome par_lookup.csv --> check if file
  # name is given or add if per name is not ambiguous!

  is_correct_file <- model_par$file_name %in%
    c("pnd", "rte", "sub", "swq", "hru", "gw", "sdr", "sep", "bsn", "wwq",
      "res", "ops", "sol", "mgt", "chm", "swq", "hlt", "plt", "pst", "cli", "aqu")

  if(any(!is_correct_file)) {
    stop(paste(model_par$file_name[!is_correct_file], collapse = ", ")%&&%
         "files are no valid file type!")
  }

  unique_par <- table(model_par$par_name)
  if(any(unique_par > 1)) {
    stop("Duplicated parameter names found! Define individual names with:\n"%&%
         "par_name::parameter|...")
  }

  file_expr <- "filter(., file_name == '"%&%model_par$file_name%&%"')"

  is_sub_par <- model_par$file_name %in% c("pnd", "rte", "sub", "swq")
  is_bsn_par <- model_par$file_name %in% c("bsn", "wwq", "res", "ops", "hlt", "plt", "pst", "cli")


  par_clean <- par %>% strsplit(., "\\|")
  chg_pos <- map(par_clean, ~ which(grepl("change", .x)))
  sub_pos <- map(par_clean, ~ which(grepl("sub", .x))) %>%
    map2(., chg_pos, ~ c(1:.y, .x)) %>%
    map(., unique)

  par_clean[is_bsn_par] <- map2(par_clean[is_bsn_par],chg_pos[is_bsn_par], ~.x[1:.y])
  par_clean[is_sub_par] <- map2(par_clean[is_sub_par],sub_pos[is_sub_par], ~.x[.y])

  filter_expr <- par_clean %>%
    map(., ~ .x[2:length(.x)] %>% .[!grepl("change",.)])

  filter_var_val <- map(filter_expr, ~ str_split(.x, bool_op))
  filter_var <- map(filter_var_val, ~unlist(.x)[1]) %>%
    map(., trimws) %>%
    map(., tolower)
  filter_val <-  map(filter_var_val, ~unlist(.x)[2]) %>%
    map(., ~ gsub("c\\(|\\)", "", .x)) %>%
    map(., ~ add_quotes_if_chr(.x)) %>%
    map(., ~ concat_values(.x))
  filter_op  <- map(filter_expr, ~ str_extract(.x, bool_op)) %>%
    map(., ~ trimws(.x)) %>%
    map(., ~ gsub("\\=", "\\=\\=", .x )) %>%
    map(., ~ gsub("\\=\\=\\=\\=", "\\=\\=", .x ))

  expressions <- pmap(list(filter_var, filter_val, filter_op), build_expr) %>%
    map_df(., as_tibble) %>%
    mutate(file_expression = paste(file_expr, file_expression, sep = " %>% ") %>%
             gsub(" %>% $", "", .))

  model_par <- map2(filter_op, filter_val, paste) %>%
    map2(.,filter_var, ~ as.list(c(NA,.x)) %>% set_names(., c("idx",.y)) %>% as_tibble(.)) %>%
    bind_rows(.) %>%
    select(-idx) %>%
    select(one_of("sub", "hru", "luse", "soil", "slope"), everything()) %>%
    bind_cols(model_par, ., expressions)

  return(model_par)
}

#' Add quotes to character values of vector in filter expression
#'
#' @param chr Character string providing values for filter variable
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map map_chr map_lgl map_if
#' @keywords internal
#'
add_quotes_if_chr <- function(chr) {
  is_chr <- chr %>%
    strsplit(., ",|:") %>%
    map(., ~ as_num(.x)) %>%
    map_lgl(.,  ~ is.na(.x) %>% any(.))

  chr %>%
    strsplit(., ",") %>%
    map(., ~ trimws(.x)) %>%
    map_if(., is_chr, ~ paste0("'",.x, "'")) %>%
    map_chr(., ~ paste(.x, collapse = ", ")) %>%
    gsub("\\'\\'", "\\'",.)
}

#' Helper function to wrap text string with "c(...)" to concatenate values
#'
#' @param x Text string
#' @keywords internal
#'
concat_values <- function(x){
  x[!grepl("c\\((.*?)\\)",x) & grepl("\\,", x)] <-
    "c("%&%x[!grepl("c\\((.*?)\\)",x) & grepl("\\,", x)]%&%")"
  return(x)
}

#' Build expresions used to filter parameter values in parameter modification
#' step
#'
#' @param var List of filter variables
#' @param val List of values for the respective filter variables
#' @param op  List of logical operators for the respective filter variables
#'
#' @importFrom purrr pmap_chr
#' @keywords internal
#'
build_expr <- function(var, val, op) {
  is_file_var <- var %in% c("sub", "hru", "luse", "soil", "slope")

  file_filter <- pmap_chr(list(var[is_file_var], val[is_file_var], op[is_file_var]),
           build_filter) %>%
    paste(., collapse = " %>% ")

  var[!is_file_var] <- toupper(var[!is_file_var])
  spec_filter <- pmap_chr(list(var[!is_file_var], val[!is_file_var], op[!is_file_var]),
                          build_filter) %>%
    paste(., collapse = " %>% ")

  return(list(file_expression = file_filter, spec_expression = spec_filter))
}

#' Build filter expresions for one sequence of variable, operator and values
#'
#' @param var Character string providing the filter variable
#' @param val Character string providing the values for the respective filter
#'   variable
#' @param op  Character string providing the logical operator for the filter
#'   operation
#'
#' @importFrom purrr pmap
#' @keywords internal
#'
build_filter <- function(var, val, op) {
  if(grepl("\\,|\\:", val)){
    if(op == "==" | op == "%in%") {
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
