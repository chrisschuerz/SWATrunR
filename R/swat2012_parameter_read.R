#' Translate the parameter inputs into a parameter input table and a separarate
#' table providing the file constraints and the filter expressions for the
#' respective parameter
#'
#' @param parameter Model parameters as named vector or tibble
#'
#' @keywords internal
#'
format_parameter <- function(parameter) {
  par_constrain <- suppressWarnings(translate_parameter_constraints(names(parameter)))
  names(parameter) <- par_constrain$par_name
  if(!is.data.frame(parameter)) parameter <- map_dfc(parameter, ~.x)
  return(list(values = parameter, parameter_constrain = par_constrain))
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

#' Read the meta information for the parameter files
#'
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#'
#' @importFrom dplyr %>% left_join mutate
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @keywords internal
#'
read_file_meta <- function(project_path, par_constrain) {
  file_meta <- tibble(file = list.files(project_path) %>%
                               .[!grepl("output", .)],
                      file_code = gsub("\\..*$", "", file),
                      file_name  = gsub(".*\\.","", file)) %>%
    left_join(., read_hru(project_path), by = "file_code") %>%
    mutate(idx = 1:nrow(.))

  idx_sel <- map(par_constrain$file_expression,
                 ~ evaluate_expression(file_meta, .x) %>% .[["idx"]]) %>%
    unlist(.) %>%
    unique(.)

  file_meta %>%
    filter(., idx %in% idx_sel) %>%
    select(-idx)
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
  hru_i <- read_lines(hru_file_i)
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
#' @importFrom dplyr %>% filter
#' @importFrom pasta %//%
#' @importFrom purrr map map_dfc set_names
#' @importFrom readr read_lines
#' @importFrom tibble as_tibble
#' @keywords internal
#'
read_par_list <- function(file_meta, file_suffix, project_path){
  file_sel <- filter(file_meta, file_name == file_suffix)
  if(nrow(file_sel) > 0) {
    tmp <- read_lines(file = project_path%//%file_sel$file[1])
    par_pos <- is_par(tmp)
    par_name <- get_par_name(tmp, par_pos)

    files <- map(project_path%//%file_sel$file, read_lines)
    par_table <- map_dfc(files, ~ get_value(.x, par_pos)) %>%
      t(.) %>%
      as_tibble(.) %>%
      set_names(., par_name) %>%
      mutate(file_code = file_sel$file_code)

    return(list(file = files, value = par_table))
  }
}

#' Read parameters from the '.chm' file (as it has an individual file structure)
#'
#' @param file_meta Table that provides the file meta data
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#'
#' @importFrom dplyr %>% filter mutate bind_rows
#' @importFrom pasta %//%
#' @importFrom purrr map map2 map_dfc set_names
#' @importFrom readr read_lines
#' @keywords internal
#'
read_chm <- function(file_meta, project_path) {
  file_sel <- filter(file_meta, file_name == "chm")

  files <- map(project_path%//%file_sel$file, read_lines)

  table_pos <-  3:8
  col_pos   <-  c(28 + (0:10)*rep(12))
  par_name  <-  c("LAYER","SOL_NO3", "SOL_ORGN",
                  "SOL_LABP", "SOL_SOLP", "SOL_ORGP")

  par_table <- map(files, ~ get_table(.x, table_pos, col_pos)) %>%
    map(., ~ set_names(.x, par_name)) %>%
    map2(., file_sel$file_code, ~ mutate(.x, file_code = .y)) %>%
    bind_rows(.)

  return(list(file = files, value = par_table))
}

#' Read parameters from the '.sol' file (as it has an individual file structure)
#'
#' @param file_meta Table that provides the file meta data
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#'
#' @importFrom dplyr %>% filter mutate bind_rows left_join
#' @importFrom pasta %//%
#' @importFrom purrr map map2 map_dfc set_names
#' @importFrom readr read_lines
#' @importFrom tibble as_tibble
#' @keywords internal
#'
read_sol <- function(file_meta, project_path) {
  file_sel <- filter(file_meta, file_name == "sol")

  files <- map(project_path%//%file_sel$file, read_lines)

  par_list <- map_dfc(files, ~ gsub(".*\\:","", .x[4:6]) %>% as.numeric(.)) %>%
    t(.) %>%
    as_tibble(.) %>%
    set_names(., c("SOL_ZMX", "ANION_EXCL", "SOL_CRK")) %>%
    mutate(., file_code = file_sel$file_code)

  table_pos <-  8:21
  col_pos   <-  c(28 + (0:10)*rep(12))
  col_pos   <- col_pos[(col_pos - 1) <= nchar(files[[1]][8])]
  par_name  <-  c("SOL_Z","SOL_BD", "SOL_AWC", "SOL_K", "SOL_CBN",
                  "CLAY", "SILT", "SAND", "ROCK", "SOL_ALB", "USLE_K",
                  "SOL_EC", "SOL_PH", "SOL_CACO3")

  par_table <-  map(files, ~ get_table(.x, table_pos, col_pos)) %>%
    map(., ~ set_names(.x, par_name)) %>%
    map2(., file_sel$file_code, ~ mutate(.x, file_code = .y)) %>%
    bind_rows(.) %>%
    left_join(., par_list, by = "file_code")

  return(list(file = files, value = par_table))
}

#' Read parameters from the '.mgt' file (as it has an individual file structure)
#'
#' @param file_meta Table that provides the file meta data
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#'
#' @importFrom dplyr %>% filter mutate bind_rows
#' @importFrom pasta %//%
#' @importFrom purrr map map2 set_names
#' @keywords internal
#'
read_mgt <- function(file_meta, project_path) {
  file_list <- read_par_list(file_meta, "mgt", project_path)

  file_sel <- filter(file_meta, file_name == "mgt")
  col_pos   <-  c(1, 4, 7, 15, 17, 21, 24, 26, 38, 44, 55, 59, 65, 70)
  par_name  <- c("mon", "day", "hu", "op", "mgt"%&%1:9, "file_code")

  par_table <- map(file_list$file, ~ get_table(.x, 31:length(.x), col_pos)) %>%
    map(., t) %>%
    map(., as_tibble) %>%
    map2(., file_sel$file_code, ~ mutate(.x, file_code = .y)) %>%
    bind_rows(.) %>%
    set_names(., par_name)

  file_list$mgt_table <- par_table
  return(file_list)
}

#' Convert character string to numeric without displaying warnings
#'
#' @param chr Character string
#' @keywords internal
#'
as_num <- function(chr) {suppressWarnings(as.numeric(chr))}

#' Check which row in a character vector file holds a model parameter
#'
#' @param chr Character string
#' @keywords internal
#'
is_par <- function(chr) {
  num <- chr %>% substr(., 1, 16) %>% as_num(.)
  return(!is.na(num))
}

#' Extract the parameter name from one line in a list parameter file
#'
#' @param chr Character string
#' @param par_pos Logical vector that provides if a row holds a parameter or not
#' @keywords internal
#'
get_par_name <- function(chr, par_pos) {
  gsub("\\:.*$", "", chr) %>%
    gsub(".*\\|","", .) %>%
    trimws(.) %>%
    .[par_pos]
}

#' Extract the parameter values from a list parameter file
#'
#' @param file_i The i'th parameter file for a file suffix
#' @param par_pos Logical vector that provides if a row holds a parameter or not
#' @keywords internal
#'
get_value <- function(file_i, par_pos) {
  file_i[c(par_pos, rep(FALSE, (length(file_i) - length(par_pos))))] %>%
    substr(., 1, 16) %>%
    as_num(.)
}

#' Extract the parameter values that are provided in tabular form in a file
#'
#' @param file_i The i'th parameter file for a file suffix
#' @param table_pos Index vector indicating which lines belong to the table
#' @param col_pos Index vector that separates the individual table columns
#' @importFrom purrr map_dfc
#' @keywords internal
#'
get_table <- function(file_i, table_pos, col_pos) {
  col_start <- col_pos[1:(length(col_pos)-1)]
  col_end   <- col_pos[2:length(col_pos)] - 1
  file_i[table_pos] %>%
    map_dfc(., ~ split_line(.x, col_start, col_end))
}

#' Split one line in a parameter file into the individual values of the table
#'
#' @param file_i The i'th parameter file for a file suffix
#' @param start Index vector indicating the start values of a value
#' @param end Index vector indicating the end values of a value
#' @importFrom purrr map2_chr
#' @keywords internal
#'
split_line <- function(chr, start, end) {
  map2_chr(start, end, ~ substr(chr, .x, .y)) %>%
    as.numeric(.)
}
