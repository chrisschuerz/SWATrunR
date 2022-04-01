# Functions for reading SWAT+ outputs

#' Read SWAT+ output files
#'
#' @param output Output defined to read from the SWAT model results
#' @param thread_path Path to respective thread where SWAT was executed
#' @param revision Numeric. The revision number of the SWAT+ executable
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map map2 map_chr set_names
#' @importFrom readr fwf_positions read_fwf
#' @keywords internal
#'
read_swatplus_output <- function(output, thread_path, add_date, revision) {

  if(add_date){
    date_cols <- c('yr', 'mon', 'day')
  } else {
    date_cols <- c()
  }
  output <- output %>% group_by(file) %>% group_split()

  unit_names <- output %>%
    map(., ~ fread(thread_path%//%.x$file[1], skip = 2, nrows = 1, header = F)) %>%
    map(., ~ unlist(.x) %>% unname(.))

  col_names <- output %>%
    map(., ~ fread(thread_path%//%.x$file[1], skip = 1, nrows = 1, header = F)) %>%
    map(., ~ unlist(.x) %>% unname(.)) %>%
    map2(., unit_names, ~ replace_colname_na(.x, .y)) %>%
    map(., ~ .x[!is.na(.x)]) %>%
    map(., ~add_suffix_to_duplicate(.x))

  ## Read all output files, assign column names and assign output file names
  out_tables <- map2(output, col_names, ~ read_output_i(.x, .y, thread_path, date_cols))

  if(add_date) {
    date <- out_tables[[1]] %>%
      filter(unit == out_tables[[1]]$unit[1]) %>%
      mutate(date = ymd(paste(yr,mon,day, sep = '-'))) %>%
      select(date)

    out_tables <- map(out_tables, ~ select(.x, -yr, -mon, -day))
  }

  out_tables <- out_tables %>%
    map(., ~ add_id(.x)) %>%
    map2(., output, ~mutate_output_i(.x, .y)) %>%
    bind_cols(.)

  if(add_date) {
    out_tables <- bind_cols(date, out_tables)
  }

  return(out_tables)
}


#' Reading the i_th SWAT+ output file, filter required units and select variables.
#'
#' @param output_i i_th part from the output table which defines what to
#'   read from the SWAT model results
#' @param col_names_i Prepared and fixed column names of output file i
#' @param thread_path String path to the thread where to read the output file
#' @param date_cols If data should be read, vector of names of the date columns.
#'
#' @importFrom data.table fread
#' @importFrom dplyr filter select %>%
#' @importFrom purrr set_names
#' @importFrom tibble as_tibble
#' @importFrom tidyselect all_of
#' @keywords internal
#'
read_output_i <- function(output_i, col_names_i, thread_path, date_cols) {
  fread(thread_path%//%output_i$file[1], skip = 3) %>%
    as_tibble(.) %>%
    .[,1:length(col_names_i)] %>%
    set_names(col_names_i) %>%
    select(., all_of(c(date_cols, 'unit', output_i$expr))) %>%
    filter(unit %in% (output_i$unit %>% unlist(.) %>% unique(.)))
}

#' Add suffix value to duplicated column names of SWAT+ output files..
#'
#' @param col_name Vector of column names
#'
#' @importFrom dplyr %>%
#'
#' @keywords internal
#'
add_suffix_to_duplicate <- function(col_name){
  dupl <- table(col_name) %>%
    .[. > 1]

  if(length(dupl > 0)) {
    for(i in 1:length(dupl)) {
      col_name[col_name == names(dupl[i])] <- paste0(names(dupl[i]), c('', 1:(dupl[i]-1)))
    }
  }

  return(col_name)
}

#' Add id column to output table
#'
#' @param tbl Output table
#'   read from the SWAT model results
#'
#' @importFrom dplyr mutate
#'
#' @keywords internal
#'
add_id <- function(tbl){
  mutate(tbl, id = rep(1:(nrow(tbl)/length(unique(unit))),
                         each = length(unique(unit))),
           .before = 1)
}

#' Transform extracted outputs into a wide table and add suffix to variable names.
#'
#' @param out_tbl_i i_th output table read from the SWAT+ outputs
#' @param output_i i_th part from the output table which defines what to
#'   read from the SWAT model results
#'
#' @importFrom dplyr bind_cols filter mutate select %>%
#' @importFrom purrr map map2 map_lgl set_names
#' @importFrom tidyr pivot_wider
#' @keywords internal
#'
mutate_output_i <- function(out_tbl_i, output_i) {
  is_multi_unit <- map_lgl(output_i$unit, ~ length(.x) > 1)

  map(output_i$expr, ~ select(out_tbl_i, id, unit, .x)) %>%
    map2(., output_i$unit, ~ filter(.x, unit %in% .y)) %>%
    map2(., output_i$name, ~ set_names(.x, c('id', 'unit', .y))) %>%
    map(., ~ mutate(.x, unit = paste0('_', unit))) %>%
    map2(., is_multi_unit, ~ mutate(.x, unit = ifelse(rep(.y, nrow(.x)), unit, ''))) %>%
    map(.,  ~ pivot_wider(.x, id_cols = id, names_from = unit, names_glue = "{.value}{unit}", values_from = 3)) %>%
    map(., ~ select(.x, -id)) %>%
    bind_cols()

}

#' Translate the output file settings defined according to print.prt to the
#' actual output file names
#'
#' @param output List of output variables defined with \code{define_output}
#' @param output_interval the time interval for writing the simulated outputs
#' @param revision The revision number of the SWAT+ executable
#'
#' @importFrom dplyr %>% case_when
#' @importFrom tibble tribble
#' @importFrom purrr map
#' @keywords internal
#'
translate_outfile_names <- function(output, output_interval, revision) {
  if (revision < 57) {
    translate_rev55(output, output_interval)
  } else if (revision >= 57) {
    translate_rev57(output, output_interval)
  }
}

#' Translate the output file settings defined according to print.prt to the
#' actual output file names for SWAT+ Revisions later than 56
#'
#' @param output List of output variables defined with \code{define_output}
#' @param output_interval the time interval for writing the simulated outputs
#'
#' @importFrom dplyr %>% case_when
#' @importFrom tibble tribble
#' @importFrom purrr map
#' @importFrom stringr str_sub
#' @keywords internal
#'
translate_rev57 <- function(output, output_interval) {
  output_interval <- str_sub(output_interval, 1,1) %>% tolower(.)
  output_interval <-
    case_when(output_interval == "d" ~ "_day",
              output_interval == "m" ~ "_mon",
              output_interval == "y" ~ "_yr",
              output_interval == "a" ~ "_aa")
  output <- mutate(output, file = paste0(file, output_interval, '.txt'))

  return(output)
}


#' Translate the output file settings defined according to print.prt to the
#' actual output file names or SWAT+ Revisions before 56
#'
#' @param output List of output variables defined with \code{define_output}
#' @param output_interval the time interval for writing the simulated outputs
#'
#' @importFrom dplyr %>% case_when
#' @importFrom tibble tribble
#' @importFrom purrr map
#' @importFrom stringr str_sub
#' @keywords internal
#'
translate_rev55 <- function(output, output_interval) {
  output_files <- tribble(
    ~object,        ~file,       ~specifier,
    "basin_wb",     "waterbal",  "_bsn",
    "basin_nb",     "nutbal",    "_bsn",
    "basin_ls",     "losses",    "_bsn",
    "basin_pw",     "plantwx",   "_bsn",
    "basin_aqu",    "aquifer",   "_bsn",
    "basin_res",    "reservoir", "_bsn",
    "basin_cha",    "channel",   "_bsn",
    "basin_sd_cha", "channel",   "_sd_bsn",
    #"basin_psc" not found in the outputs
    #No region files were written
    "lsunit_wb",    "waterbal",  "_lsu",
    "lsunit_nb",    "nutbal",    "_lsu",
    "lsunit_ls",    "losses",    "_lsu",
    "lsunit_pw",    "plantwx",   "_lsu",
    "hru_wb",       "waterbal",  "_hru",
    "hru_nb",       "nutbal",    "_hru",
    "hru_ls",       "losses",    "_hru",
    "hru_pw",       "plantwx",   "_hru",
    #"hru-lte_XX" not found in outputs
    "channel",      "channel",   "",
    #"channel_sd" not found in outputs
    "aquifer",      "aquifer",   "",
    "reservoir",    "reservoir", "",
    # "recall" not found in outputs
    # "hyd" is very inconsistent (hydin/out)!!!
    "ru",           "routing_units", "")

  output_interval <- str_sub(output_interval, 1,1) %>% tolower(.)
  output_interval <-
    case_when(output_interval == "d" ~ "_day",
              output_interval == "m" ~ "_mon",
              output_interval == "y" ~ "_yr",
              output_interval == "a" ~ "_aa")

  map(output, function(tbl, out_files, out_int) {
    obj <- tbl$file[[1]]
    file_name <- paste0(output_files$file[output_files$object == obj],
                        output_interval,
                        output_files$specifier[output_files$object == obj],
                        ".txt")
    tbl$file <- file_name
    return(tbl)
  }, output_files, output_interval)
}

#' Remove the units from variable names in output files of SWAT+ Revisions before 56
#'
#' @param col_nm Character vector with column names
#'
#' @keywords internal
#'
remove_units_plus <- function(col_nm) {
  unit <- "\\_ha\\-m|\\_ha|\\_tons|\\_mtons|\\_ton|\\_mg\\/|\\_mg|\\_kgN\\/ha|\\_kgP\\/ha|\\_kgP|\\_kgN|\\_kg|\\_k|_mgpst|\\_mgps|\\_m\\/m|\\_mm|\\_m|\\_m\\^3\\/s|\\_m\\^3|\\_sedm|\\_burym|\\_\\#cfu\\/100ml|\\_\\#cfu\\/100m|\\_degC|\\_degc|\\/deg|\\_tha|\\_kgha|\\_mj\\/m\\^2|\\_m\\/s|\\_frac"
  col_nm <- gsub(unit, "", col_nm) %>%
    gsub("\\_$", "", .)
  return(col_nm)
  }

#' Fix issues with shifted col_names in SWAT+ rev59.3
#'
#' @param col_nm Character vector with column names
#' @param unit_nm Character vector with unit line from table
#'
#' @keywords internal
#'
replace_colname_na <- function(col_nm, unit_nm) {
  col_nm[is.na(col_nm)] <- unit_nm[is.na(col_nm)]
  return(col_nm)
}
