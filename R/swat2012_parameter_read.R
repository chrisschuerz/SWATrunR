#' Translate the parameter inputs into a parameter input table and a separarate
#' table providing the file constraints and the filter expressions for the
#' respective parameter
#'
#' @param parameter Model parameters as named vector or tibble
#'
#' @keywords internal
#'
format_swat2012_parameter <- function(parameter, swat_vers) {
  par_names <- names(parameter)
  if(!any(par_names %in% c("values", "definition"))) {
    par_constrain <- translate_parameter_constraints(par_names, swat_vers)
    names(parameter) <- par_constrain$par_name
    if(!is.data.frame(parameter)) parameter <- map_dfc(parameter, ~.x)
    return(list(values = parameter, definition = par_constrain))
  } else {
    return(parameter)
    }
}

#' Read the original swat parameter values from the parameter files in
#' project_path
#'
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#' @param par_constrain Table providing the file constraints for the respective
#'   parameter that will be modified
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map set_names
#' @keywords internal
#'
read_swat2012_files <- function(project_path, file_meta) {

  list_par_files <- c("pnd", "rte", "sub", "swq", "hru", "gw",
                      "sdr", "sep", "bsn", "wwq", "res", "ops")

  list_par_files <- list_par_files[list_par_files %in% file_meta$file_name]

  backup <- map(list_par_files, ~ read_par_list(file_meta, .x, project_path)) %>%
    set_names(., list_par_files)
  if("chm" %in% file_meta$file_name) {
    backup$chm <- read_chm(file_meta, project_path)
  }
  if("sol" %in% file_meta$file_name) {
    backup$sol <- read_sol(file_meta, project_path)
  }
  if("mgt" %in% file_meta$file_name) {
    backup$mgt <- suppressWarnings(read_mgt(file_meta, project_path))
  }

  return(backup)
}
#
# c("pnd", "rte", "sub", "swq", "hru", "gw", "mgt", "sol", "chm",
#   "sdr", "sep", "bsn", "wwq", "res", "ops")

#' Read the meta information for the parameter files
#'
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#'
#' @importFrom dplyr %>% distinct left_join mutate
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @importFrom stringr str_remove str_sub
#' @importFrom tidyselect any_of
#' @keywords internal
#'
read_file_meta <- function(project_path, par_constrain) {
  files <- list.files(project_path) %>%
    .[!grepl('output',.)]
  file_meta <- tibble(file = files,
                      file_code = str_remove(file, "\\..*$"),
                      file_name  = str_remove(file, ".*\\.")) %>%
    left_join(., read_hru(project_path), by = "file_code") %>%
    mutate(sub = str_sub(file_code, 1,5) %>% as_num(.),
           sub = ifelse(sub > 0, sub, NA))
  par_constrain %>%
    build_expression(.) %>%
    map_df(., ~ evaluate_expression(file_meta, .x)) %>%
    distinct(., file, .keep_all = T)
}

#' Read all '.hru' files in the project_path and extract the meta data from
#' these files and return them as a meta data tibble
#'
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map_df
#' @keywords internal
#'
read_hru <- function(project_path) {
  hru_files <- list.files(path = project_path, pattern = ".hru$", full.names = TRUE) %>%
    .[!grepl("output", .)]

  map_df(hru_files, get_hru_meta)
}

#' Extract the meta data from the header of a '.hru' file
#'
#' @param hru_file_i The i'th hru file (read as character vector)
#'
#' @importFrom dplyr %>%
#' @importFrom readr read_lines
#' @importFrom tibble tibble
#' @keywords internal
#'
get_hru_meta <- function(hru_file_i) {
  hru_i <- read_lines(hru_file_i, lazy = FALSE)
  hru_i_head <- unlist(strsplit(hru_i[1], "\\ |\\:|\\: "))
  tibble(file_code = (basename(hru_file_i) %>% gsub(".hru$", "", .)),
         hru       = as.numeric(hru_i_head[which(hru_i_head == "HRU")[1]+1]),
         sub       = as.numeric(hru_i_head[which(hru_i_head == "Subbasin")+1]),
         hru_sub   = as.numeric(hru_i_head[which(hru_i_head == "HRU")[2]+1]),
         luse      = hru_i_head[which(hru_i_head == "Luse")+1],
         soil      = hru_i_head[which(hru_i_head == "Soil")+1],
         slope     = hru_i_head[which(hru_i_head == "Slope")+1])
}

#' Read parameters that are arranged in a simple list (1 parameter per row)
#'
#' @param file_meta Table that provides the file meta data
#' @param file_suffix Suffix of the parameter files from which parameters are
#'   read
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#'
#' @importFrom dplyr %>% filter mutate
#' @importFrom purrr map map_df map_int set_names
#' @importFrom readr guess_encoding locale read_lines
#' @importFrom stringr str_length str_locate str_locate_all str_sub
#' @importFrom tibble as_tibble
#' @keywords internal
#'
read_par_list <- function(file_meta, file_suffix, project_path, n_row = NULL){
  file_sel <- filter(file_meta, file_name == file_suffix)
  if(nrow(file_sel) > 0) {
    enc <- guess_encoding( project_path%//%file_sel$file[1])
    tmp <- read_lines(file = project_path%//%file_sel$file[1],
                      locale = locale(encoding = enc[[1]][1]),
                      lazy = FALSE)
    tmp <- tmp[1:min(length(tmp), n_row)]
    par_pos <- is_par(tmp)
    par_name <- get_par_name(tmp, par_pos)
    sep_pos <- str_locate(tmp[par_pos], '\\|')[,2]
    val_pos <- tmp[par_pos] %>%
      str_sub(., 1, sep_pos-1) %>%
      str_locate_all(., '[:digit:]') %>%
      map_int(., max)
    par_txt <- str_sub(tmp[par_pos], val_pos+1, str_length(tmp[par_pos]))

    files <- map(project_path%//%file_sel$file, ~ read_lines(file = .x, lazy = FALSE))
    if(!is.null(n_row)) {
      files_tbl <- map(files, ~.x[1:n_row])
    } else {
      files_tbl <- files
    }

    par_table <- map(files_tbl, ~ get_value(.x, par_pos)) %>%
      map_df(., ~set_names(.x, par_name)) %>%
      mutate(file_code = file_sel$file_code) %>%
      mutate(idx = 1:nrow(.))

    return(list(file = files, value = par_table, par_txt = par_txt, val_pos = val_pos))
  }
}

#' Read parameters from the '.chm' file (as it has an individual file structure)
#'
#' @param file_meta Table that provides the file meta data
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#'
#' @importFrom dplyr %>% filter mutate bind_rows
#' @importFrom purrr map map2 map_dfc set_names
#' @importFrom readr read_lines
#' @keywords internal
#'
read_chm <- function(file_meta, project_path) {
  file_sel <- filter(file_meta, file_name == "chm")

  files <- map(project_path%//%file_sel$file, ~ read_lines(file = .x, lazy = FALSE))

  table_pos <-  3:8
  col_pos   <-  c(28 + (0:10)*rep(12))
  par_name  <-  c("LAYER","SOL_NO3", "SOL_ORGN",
                  "SOL_LABP", "SOL_SOLP", "SOL_ORGP")

  par_table <- map(files, ~ get_table(.x, table_pos, col_pos, par_name, cbind)) %>%
    map2(., file_sel$file_code, ~ mutate(.x, file_code = .y)) %>%
    bind_rows(.) %>%
    mutate(idx = 1:nrow(.))

  return(list(file = files, value = par_table))
}

#' Read parameters from the '.sol' file (as it has an individual file structure)
#'
#' @param file_meta Table that provides the file meta data
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#'
#' @importFrom dplyr %>% filter mutate bind_rows left_join
#' @importFrom purrr map map2 map_dfc set_names
#' @importFrom readr read_lines
#' @importFrom tibble as_tibble
#' @keywords internal
#'
read_sol <- function(file_meta, project_path) {
  file_sel <- filter(file_meta, file_name == "sol")

  files <- map(project_path%//%file_sel$file, ~ read_lines(file = .x, lazy = FALSE))

  par_list <- map(files, ~ gsub(".*\\:","", .x[4:6]) %>% as.numeric(.)) %>%
    reduce(., rbind) %>%
    set_colnames(., c("SOL_ZMX", "ANION_EXCL", "SOL_CRK")) %>%
    as_tibble(.) %>%
    mutate(., file_code = file_sel$file_code)

  table_pos <-  8:21
  col_pos   <-  c(28 + (0:10)*rep(12))
  par_name  <-  c("SOL_Z","SOL_BD", "SOL_AWC", "SOL_K", "SOL_CBN",
                  "CLAY", "SILT", "SAND", "ROCK", "SOL_ALB", "USLE_K",
                  "SOL_EC", "SOL_PH", "SOL_CACO3")

  par_table <-  map(files, ~ get_table(.x, table_pos, col_pos, par_name, cbind)) %>%
    map(., ~ mutate(.x, LAYER = 1:nrow(.x))) %>%
    map2(., file_sel$file_code, ~ mutate(.x, file_code = .y)) %>%
    bind_rows(.) %>%
    left_join(., par_list, by = "file_code") %>%
    filter(., !is.na(SOL_Z)) %>%
    mutate(idx = 1:nrow(.))

  return(list(file = files, value = par_table))
}

#' Read parameters from the '.mgt' file (as it has an individual file structure)
#'
#' @param file_meta Table that provides the file meta data
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#'
#' @importFrom dplyr %>% filter mutate bind_rows
#' @importFrom purrr map map2 set_names
#' @importFrom stringr str_sub
#' @keywords internal
#'
read_mgt <- function(file_meta, project_path) {
  file_list <- read_par_list(file_meta, "mgt", project_path, n_row = 27)

  file_sel <- filter(file_meta, file_name == "mgt")
  col_pos   <-  c(1, 4, 7, 16, 19, 24, 28, 31, 44, 51, 63, 68, 75, 81)
  par_name  <- c("MON", "DAY", "HU", "MGT_OP", "MGT"%&%1:9)

  par_table <- map(file_list$file, ~ get_table(.x, 31:length(.x), col_pos, par_name, rbind)) %>%
    map2(., file_sel$file_code, ~ mutate(.x, file_code = .y)) %>%
    bind_rows(.) %>%
    mutate(idx = 1:nrow(.))

  file_list$mgt_table <- par_table
  return(file_list)
}

#' Check which row in a character vector file holds a model parameter
#'
#' @param chr Character string
#' @importFrom stringr str_sub
#' @keywords internal
#'
is_par <- function(chr) {
  sep_pos <- str_locate(chr, '\\|')[,1]
  num <- chr %>% str_sub(., 1, sep_pos-1) %>%
    as_num(.)
  has_par_name <- grepl("\\:", chr) & grepl("\\:", chr)
  is_par <- !is.na(num) & has_par_name & !is.na(sep_pos)
  return(is_par)
}

#' Extract the parameter name from one line in a list parameter file
#'
#' @param chr Character string
#' @param par_pos Logical vector that provides if a row holds a parameter or not
#'
#' @importFrom stringr str_remove str_remove_all
#' @keywords internal
#'
get_par_name <- function(chr, par_pos) {
  chr[par_pos] %>%
    str_remove(., ".*\\|") %>%
    trimws(.) %>%
    str_remove(., "[:space:]+.*") %>%
    str_remove_all(., "\\:.*$")
}

#' Extract the parameter values from a list parameter file
#'
#' @param file_i The i'th parameter file for a file suffix
#' @param par_pos Logical vector that provides if a row holds a parameter or not
#' @importFrom stringr str_sub
#' @keywords internal
#'
get_value <- function(file_i, par_pos) {
  par_pos <- c(par_pos[1:min(length(file_i), length(par_pos))],
               rep(FALSE, max(0, length(file_i) - length(par_pos))))
  sep_pos <- str_locate(file_i, '\\|')[par_pos,1]
  file_i[c(par_pos, rep(FALSE, (length(file_i) - length(par_pos))))] %>%
    str_sub(., 1, sep_pos - 1) %>%
    as_num(.)
}

#' Extract the parameter values that are provided in tabular form in a file
#'
#' @param file_i The i'th parameter file for a file suffix
#' @param table_pos Index vector indicating which lines belong to the table
#' @param col_pos Index vector that separates the individual table columns
#' @importFrom purrr map reduce
#' @importFrom tibble as_tibble
#' @keywords internal
#'
get_table <- function(file_i, table_pos, col_pos, col_names, fun) {
  col_start <- col_pos[1:(length(col_pos)-1)]
  col_end   <- col_pos[2:length(col_pos)] - 1

  if(length(file_i) < table_pos[1]) {
    tbl_out <- matrix(data = NA, nrow = 1, ncol = length(col_names)) %>%
      as_tibble(., .name_repair = "minimal") %>%
      set_names(col_names)
  } else if(length(file_i) == table_pos[1]) {
    tbl_out <- matrix(data = split_line(file_i[table_pos], col_start, col_end),
                      nrow = 1, ncol = length(col_names)) %>%
      as_tibble(., .name_repair = "minimal") %>%
      set_names(col_names)
  } else {
    tbl_out <- file_i[table_pos] %>%
      map(., ~ split_line(.x, col_start, col_end)) %>%
      reduce(., fun) %>%
      set_colnames(., col_names) %>%
      as_tibble(.)
  }
  return(tbl_out)
}

#' Split one line in a parameter file into the individual values of the table
#'
#' @param file_i The i'th parameter file for a file suffix
#' @param start Index vector indicating the start values of a value
#' @param end Index vector indicating the end values of a value
#' @importFrom purrr map2_chr
#' @importFrom stringr str_sub
#' @keywords internal
#'
split_line <- function(chr, start, end) {
  map2_chr(start, end, ~ str_sub(chr, .x, .y)) %>%
    as.numeric(.)
}

#' Set the column names of a matrix
#'
#' @param mtx The matrix
#' @param col_names Character vector with column names
#' @keywords internal
#'
set_colnames <- function(mtx, col_names) {
  colnames(mtx) <- col_names
  return(mtx)
}
