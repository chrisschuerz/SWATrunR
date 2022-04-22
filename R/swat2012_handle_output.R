# Functions for reading SWAT outputs

#' Read SWAT output files
#'
#' @param output Output defined to read from the SWAT model results
#' @param thread_path Path to respective thread where SWAT was executed
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map map2 map_chr pmap set_names
#' @importFrom readr fwf_positions read_fwf
#' @keywords internal
#'
read_swat2012_output <- function(output, thread_path) {
  ## Get unique output files defined in output
  output_files <- unique(output$file)

  ## Find the first position of table in each file
  frst_pos <- find_first_line(output_files, thread_path)
  ## Get the column header for all output files
  file_header <- map2(output_files, frst_pos,
                        ~ get_file_header(.x, .y, thread_path))
  ## Get the variable positions in all output files
  fwf_pos     <- map2(output_files, frst_pos, ~ get_fwf_positions(.x, thread_path, .y))

  # Read all output files, assign column names and assign output file names
  out_tables <- pmap(list(output_files, fwf_pos, frst_pos),
                     function(out, fwf, frst, thread_path) {
                       read_fwf(file = thread_path%//%out,
                                col_positions = fwf_positions(fwf[[1]], fwf[[2]]),
                                skip = frst, guess_max = 3, lazy = FALSE)}, thread_path) %>%
    map2(., file_header, ~set_names(.x, .y)) %>%
    set_names(., output_files)

  tables_nrow <- map(out_tables, ~nrow(.x)) %>% unlist(.)
  if(any(tables_nrow == 0)){
    stop("\nOne of the SWAT runs was not successful!\n"%&&%
         "The defined model parameters could be a reason.\n"%&&%
         "Please check if any change in the model parametrization"%&&%
         "caused a parameter to be out of bounds!")
  }

  return(out_tables)
}

#' Read the column names for the SWAT output files
#'
#' @param output Output defined to read from the SWAT model results
#' @param fwf_pos Fixed width positions for the variables in the output files
#' @param thread_path Path to respective thread where SWAT was executed
#'
#' @importFrom dplyr %>%
#' @importFrom readr read_lines
#' @keywords internal
#'
get_file_header <- function(output_i, tbl_pos, thread_path) {
  header <- read_lines(file = thread_path%//%output_i,
                       skip = tbl_pos - 1, n_max = 1, lazy = FALSE) %>%
    split_by_units(.) %>%
    str_replace_all(., "-", "_") %>%
    str_replace_all(., "#", "_")

  if(output_i != "output.hru") {
    header <- c("FILE", header)
  }
  return(header)
}

#' Derive the fixed with positions of the columns in the SWAT output files
#'
#' @param output Output defined to read from the SWAT model results
#' @param thread_path Path to respective thread where SWAT was executed
#'
#' @importFrom dplyr %>%
#' @importFrom readr read_lines
#' @importFrom stringr str_detect str_locate str_locate_all str_sub
#' @keywords internal
#'
get_fwf_positions <- function(output_i, thread_path, tbl_pos) {
  header_line <- read_lines(file = thread_path%//%output_i,
                            skip = tbl_pos - 1, n_max = 1, lazy = FALSE)
  first_line <- read_lines(file = thread_path%//%output_i,
                           skip = tbl_pos, n_max = 1, lazy = FALSE)

  # Workaround to split MON and AREA flexibly
  pos_mon_area <- c(str_locate(header_line, "MON")[1],
                    str_locate(header_line, "AREA")[2])

  if(any(is.na(pos_mon_area))) {
    start_pos <- c(1, str_locate_all(first_line, " +")[[1]][,1])

  } else {
    chr_mon_area <- str_sub(first_line, pos_mon_area[1], pos_mon_area[2])

    chr_split <- chr_mon_area %>%
      trimws(.) %>%
      str_detect(., " ") %>%
      ifelse(., " +", "\\.")

    pos_split <- (str_locate_all(chr_mon_area, chr_split)[[1]] + pos_mon_area[1] - 1) %>%
      .[nrow(.),1] %>%
      unname(.)

    start_pos <- str_locate_all(first_line, " +")[[1]][,1] %>%
      .[!(. %in% pos_mon_area[1]:pos_mon_area[2])] %>%
      c(1, pos_split, .) %>%
      sort(.)
  }

  if(output_i != "output.hru") {
    last_val <- (str_locate_all(first_line, "E")[[1]][,1] + 4) %>%
      .[length(.)]
  } else {
    last_val <- nchar(first_line)
  }


  start_pos <- start_pos[start_pos < last_val]

  end_pos <- c(start_pos[2:length(start_pos)], last_val) - 1
  return(list(start_pos, end_pos))
}

#' Helper function to find the position of the fist line of the table in a file
#'
#' @param out_file The output files to be read
#' @param thread_path Path to respective thread where SWAT was executed
#'
#' @importFrom purrr map map_int
#' @importFrom readr read_lines
#' @keywords internal
#'
find_first_line <- function(out_file, thread_path) {
  file_head <- map(out_file, ~ read_lines(thread_path%//%.x, n_max = 50, lazy = FALSE))
  head_line <- map_int(file_head, ~ which(grepl("MON", .x)))
  return(head_line)
}

#' Helper function to convert file.cio entries to numerics
#'
#' @param cio_entry Line from file.cio
#' @importFrom stringr str_sub
#'
#' @keywords internal
#'
cio_to_numeric <- function(cio_entry) {
  cio_entry %>% str_sub(., 1, 16) %>% as.numeric(.)
}

#' Remove the units from variable names in output files of SWAT2012 simulations
#'
#' @param col_nm Character vector with column names
#'
#' @keywords internal
#'
remove_units_2012 <- function(col_nm) {
  unit <- "ppm|mg\\/m3|m3|Mg\\/l$|mg\\/L$|mg\\/kg$|mg|kg\\/ha$|kg\\/h$|kg|t\\/ha$|mic\\/L$|\\(mm\\)$|kg$|cms$|tons$|ton$|mg$|mg\\/$|mm$|km2$|_tha$|_kgha$|\\_m$|\\_kgN\\/ha$|\\_kgP\\/ha$|\\_m\\^3$|ha\\-m$|_k$|mgps$|  |"
  col_nm <- gsub(unit, "", col_nm) %>%
    gsub("\\_$", "", .)
  return(col_nm)
}

#' Split header line at the positions of units and return tidy header
#'
#' @param header Character string header line
#' @importFrom stringr str_split str_replace_all
#'
#' @keywords internal
#'
split_by_units <- function(header) {
  unit <- "ppm|mg\\/m3|m3|Mg\\/l|mg\\/L|mg\\/kg|mg|kg\\/ha|kg\\/h|kg|t\\/ha|mic\\/L|\\(mm\\)|kg|cms|tons|ton|mg|mg\\/|mm|km2|_tha|_kgha|\\_m|\\_kgN\\/ha|\\_kgP\\/ha|\\_m\\^3|ha\\-m|_k|mgps|degC|degc|dgC|ct|[:space:]|MJ/m2|m"
  header %>%
    str_replace_all(., "WTAB ", "WTAB_") %>%
    str_replace_all(., "TOT ",  "TOT_") %>%
    str_replace_all(., "LAT ",  "LAT_") %>%
    str_split(., unit) %>%
    unlist(.) %>%
    trimws() %>%
    gsub(" ", "_", .) %>%
    .[nchar(.) > 0]
}
