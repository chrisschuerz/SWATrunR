# Functions for reading SWAT+ outputs

#' Read SWAT+ output files
#'
#' @param output Output defined to read from the SWAT model results
#' @param thread_path Path to respective thread where SWAT was executed
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map map2 map_chr set_names
#' @importFrom readr fwf_positions read_fwf
#' @keywords internal
#'
read_swatplus_output <- function(output, thread_path) {
  ## Get unique output files defined in output
  output_file <- map_chr(output,  ~ unique(.x$file)) %>% unique(.)

  col_names <- map(output_file, ~ read_table(thread_path%//%.x, skip = 1,
                                             n_max = 1, col_names = F)) %>%
    map(., ~ unlist(.x) %>% unname(.))

  is_col_duplicate <- map(col_names, function(x){
                                       duplicates <- table(x) %>%
                                         .[. > 1] %>%
                                         names(.)

                                       x %in% duplicates})

  ## Read all output files, assign column names and assign output file names
  out_tables <- map(output_file,~ read_table(file = thread_path%//%.x,
                                             col_names = FALSE, skip = 3, )) %>%
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
#' actual output file names for SWAT+ Revisions later than 56
#'
#' @param output List of output variables defined with \code{define_output}
#' @param output_interval the time interval for writing the simulated outputs
#'
#' @importFrom dplyr %>% case_when
#' @importFrom tibble tribble
#' @importFrom purrr map
#' @keywords internal
#'
translate_rev57 <- function(output, output_interval) {
  output_interval <- substr(output_interval, 1,1) %>% tolower(.)
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
#' actual output file names or SWAT+ Revisions before 56
#'
#' @param output List of output variables defined with \code{define_output}
#' @param output_interval the time interval for writing the simulated outputs
#'
#' @importFrom dplyr %>% case_when
#' @importFrom tibble tribble
#' @importFrom purrr map
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

  output_interval <- substr(output_interval, 1,1) %>% tolower(.)
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

