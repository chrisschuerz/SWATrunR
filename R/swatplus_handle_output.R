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
read_swatplus_output <- function(output, thread_path, revision) {
  ## Get unique output files defined in output
  output_file <- map_chr(output,  ~ unique(.x$file)) %>% unique(.)

  unit_names <- map(output_file, ~ read_table(thread_path%//%.x, skip = 2,
                                              n_max = 1, col_names = F)) %>%
    map(., ~ unlist(.x) %>% unname(.))

  col_names <- map(output_file, ~ read_table(thread_path%//%.x, skip = 1,
                                             n_max = 1, col_names = F)) %>%
    map(., ~ unlist(.x) %>% unname(.)) %>%
    map2(., unit_names, ~ replace_colname_na(.x, .y))

  if(revision < 56) {
    col_names <- map(col_names, remove_units_plus)
    line_skip <- rep(2, length(output_file))
    except_files <- grepl("reservoir|wetland", output_file)
    line_skip[except_files] <- 3
  } else {
    line_skip <- rep(3, length(output_file))
  }

  is_col_duplicate <- map(col_names, function(x){
                                       duplicates <- table(x) %>%
                                         .[. > 1] %>%
                                         names(.)

                                       x %in% duplicates})

  ## Read all output files, assign column names and assign output file names
  out_tables <- map2(output_file, line_skip, ~ read_table(file = thread_path%//%.x,
                                                 col_names = FALSE, skip = .y, )) %>%
    map2(., col_names, ~ .x[, 1:length(.y)]) %>%
    map2(., col_names, ~ set_names(.x, .y)) %>%
    map2(., is_col_duplicate, ~ .x[!.y]) %>%
    set_names(., output_file)

  tables_nrow <- map(out_tables, ~nrow(.x)) %>% unlist(.)
  if(any(tables_nrow == 0)){
    stop("\nOne of the SWAT runs was not successful!\n"%&&%
         "A reason can be the defined model parameters.\n"%&&%
         "Please check if any change in the model parametrization"%&&%
         "caused any parameter to be out of bounds!")
  }

  return(out_tables)
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
  out_files <- map(output, ~ paste0(.x$file[[1]], output_interval, ".txt"))
  map2(output, out_files, function(out, out_file){
    out$file <- out_file
    return(out)})
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
