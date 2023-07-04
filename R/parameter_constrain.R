#' Translate the parameter inputs and the set constraints into lookup table
#'
#' @param par Character string vector providing parameters and constraints
#'
#' @importFrom dplyr %>% bind_cols last_col relocate select
#' @importFrom purrr map map_chr map_df map_if map2_lgl map2 map2_chr
#' @importFrom tibble tibble
#' @importFrom stringr str_detect str_extract str_remove str_remove_all str_split
#' @keywords internal
#'
translate_parameter_constraints <- function(par, swat_vers) {

  has_par_name <- str_detect(par, "\\:\\:")
  par_list <- str_split(par, '\\:\\:|\\|')

  has_change   <- map2_lgl(par_list, has_par_name, ~ str_detect(.x[2+.y], "change"))
  if(any(!has_change)) {
    stop("Type of change definition is incorrect for the following parameters:\n  ",
         map_chr(par_list[!has_change], ~.x[1]) %>% paste(., collapse = ", "), "\n  ",
         "The parameter change must be provided for all parameters at the second\n  ",
         "position of the parameter name (after the first '|', e.g. 'par | change = abschg').")
  }

  par_name <- map(par_list, ~.x[1]) %>%
    map_if(., !has_par_name, ~ str_remove(.x, '\\..*')) %>%
    unlist() %>%
    trimws(.)
  parameter <- map2_chr(par_list, has_par_name, ~.x[1 + .y]) %>%
    map_chr(., ~ str_remove(.x, '\\..*')) %>%
    trimws(.)
  file_name <- map2_chr(par_list, has_par_name, ~.x[1 + .y]) %>%
    map_chr(., ~ str_remove(.x, '.*\\.')) %>%
    trimws(.)
  change <- par_list %>%
    map2_chr(., has_par_name, ~ str_remove_all(.x[2+.y], 'change|\\=')) %>%
    trimws(.)
  model_par <- tibble(par_name, parameter, file_name, change, full_name = par)

  if(any(!(change %in% c("relchg", "pctchg", "abschg", "absval")))) {
    stop("Wrong input for change type. Must be either"%&&%
         "'relchg', 'pctchg', 'abschg', or 'absval'.")
  }

  if(swat_vers == '2012') {
    swat_files <- c("pnd", "rte", "sub", "swq", "hru", "gw", "mgt", "sol", "chm",
                    "sdr", "sep", "bsn", "wwq", "res", "ops")
  } else {
    swat_files <- c("hru", "sol", "bsn", "swq", "rte", "res", "aqu", "hlt", "pst", "plt")
  }

  if(any(!(file_name %in% swat_files))) {
    stop(paste(file_name[!(file_name %in% swat_files)], collapse = ", ")%&&%
         "files are no valid file type!")
  }
  unique_par <- table(par_name)
  if(any(unique_par > 1)) {
    stop("Duplicated parameter names found! Define individual names with:\n"%&%
         "par_name::parameter|...")
  }

  bool_op <- c("\\=\\=", "\\!\\=", "\\<\\=", "\\>\\=", "\\=", "\\<", "\\>", "\\%in\\%")

  cons_list <- map2(par_list, has_par_name, ~.x[-(1:(2+.y))])

  constraints <- cons_list %>%
    map_df(., ~ build_constraint_tbl(.x, bool_op)) %>%
    remove_dummy() %>%
    map_df(., ~map_chr(.x, ~ tidy_constraint(.x, bool_op)))

  if(ncol(constraints) > 0) {
    if(swat_vers == '2012') {
      cons_var <- c("sub", "hru", "luse", "soil", "slope", 'layer')
    } else {
      # stop('Parameter constraints not yet implemented!')
      cons_var <- c('unit', 'lyr', 'year', 'day', 'hsg', 'plant', 'texture', 'landuse', 'slope') # according to Nancy also 'region' >> check at later step, check also conditions for year and day with Nancy
      # cons_var <- c("hru", "sol", "bsn", "swq", "rte", "res", "aqu", "hlt", "pst")
    }
    if(any(!(names(constraints) %in% cons_var))) {
      stop('The parameter constraints: ', paste(names(constraints)[!(names(constraints)
                                            %in% cons_var)], collapse = ", "),
           " are not valid!")
    }
    model_par <- bind_cols(model_par, constraints) %>%
      relocate(., full_name, .after = last_col())
  }
  # is_constraint_var <- names(constraints) %in% c("sub", "hru", "luse", "soil", "slope")

  # if (!) {
  #   stop("The")
  # }


  return(model_par)
}



#' Build the parameter constraint from the rules in the parameter names
#'
#' @param cons_i Text string that defines the constraint i
#' @param bool_op Vector of strings that define the different possible boolean operations
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map_df set_names
#' @importFrom stringr str_extract str_remove
#' @keywords internal
#'
build_constraint_tbl <- function(cons_i, bool_op){
  if(all(is.na(cons_i))) {
    tibble(dummy = NA)
  } else {
    rex <- paste(bool_op%.%"*", collapse = "|")
    cons_names <- str_remove(cons_i, rex) %>%
      trimws()

    str_extract(cons_i, rex) %>%
      set_names(., cons_names) %>%
      map_df(., ~.x)
  }
}

#' Apply a set of operations to the rule strings to check and clean the defined operations
#'
#' @param chr Text string that defines the constraint i
#' @param bool_op Vector of strings that define the different possible boolean operations
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map map_chr map_lgl map_if
#' @importFrom stringr str_extract str_remove str_remove_all str_split
#' @keywords internal
#'
tidy_constraint <- function(chr, bool_op) {
  rex <- paste(bool_op, collapse = "|")
  op <- str_extract(chr, rex) %>%
    ifelse(. == '=', '==', .)
  chr_sep <- str_remove(chr, rex)
  num_eval <- try(eval(parse(text = chr_sep)), silent = T)
  is_num <- is.numeric(num_eval)

  if(is.na(chr)){
    cons_tidy <- NA
  } else if(is_num) {
    op_is_eq <- op %in% c('%in%', '==')
    if(length(num_eval) == 1) {
      cons_tidy <- paste0(op,chr_sep)
    } else if (length(num_eval) > 1 & op_is_eq) {
      cons_tidy <- paste0('%in%',chr_sep)
    } else {
      stop("The parameter constraint '", chr, "' is not well defined.")
    }
  } else {
    is_op_chr <- op %in% c('%in%', '==', '!=')
    if(!is_op_chr) {
      stop("Selected boolean operation '", op, "' in '", chr, "' is not applicable to characters.")
    }
    chr_sep <- chr_sep %>%
      str_remove(., rex) %>%
      str_remove_all(., 'c\\(|\\)') %>%
      trimws(.) %>%
      str_split(., '[:space:]*,+[:space:]*| [:space:]+', simplify = T) %>%
      str_remove_all(., "\'|\"") %>%
      paste0("'",., "'")

    if(length(chr_sep) == 1) {
      cons_tidy <- paste0(op, chr_sep)
    } else {
      cons_tidy <- paste0('%in% ', 'c(', paste(chr_sep, collapse = ","), ')')
    }
  }

  return(cons_tidy)
}

#' Remove the generated dummy column that was required for the col bind
#'
#' @param tbl Tibble
#' @importFrom dplyr select
#' @keywords internal
#'
remove_dummy <- function(tbl) {
  if('dummy' %in% names(tbl)) {
    select(tbl, -dummy)
  } else {
    tbl
  }
}
