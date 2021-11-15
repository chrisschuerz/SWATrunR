#' Write modified SWAT parameter files in respective thread folder
#'
#' @param file_meta Table that provides the parameter file meta data
#' @param thread_parameter Modified model parameters to be written to respective
#'   thread folder
#' @param thread_path Path to the thread folder where model is executed
#'
#' @keywords internal
#'
write_parameter <- function(file_meta, thread_parameter, thread_path) {
  file_names <- names(thread_parameter)

  for(i_file in file_names) {
    if(i_file == "chm") {
      write_chm(file_meta, thread_parameter, thread_path)
    } else if (i_file == "sol") {
      write_sol(file_meta, thread_parameter, thread_path)
    } else if (i_file == "mgt") {
      write_mgt(file_meta, thread_parameter, thread_path)
    } else {
      write_par_list(file_meta, i_file,
                     thread_parameter, thread_path)
    }
  }
}

#' Write modified SWAT parameter files that are arranged in a linear format
#'
#' @param file_meta Table that provides the parameter file meta data
#' @param file_suffix File ending of the respective parameter files to be
#'   written
#' @param thread_parameter Modified model parameters to be written to respective
#'   thread folder
#' @param thread_path Path to the thread folder where model is executed
#'
#' @importFrom dplyr %>% filter select
#' @importFrom purrr map map2 map2_df transpose walk2
#' @importFrom readr write_lines
#' @importFrom stringr str_sub
#' @importFrom tibble as_tibble
#'
#' @keywords internal
#'
write_par_list <- function(file_meta, file_suffix,
                           thread_parameter, thread_path) {
  file_sel <- filter(file_meta, file_name == file_suffix)
  par_pos <- which(is_par(thread_parameter[[file_suffix]]$file[[1]]))

  par_files <- thread_parameter[[file_suffix]]$value %>%
    select(-idx, -file_code) %>%
    map2_df(., thread_parameter[[file_suffix]]$val_pos,
            ~sprintf(paste0("%",.y,"s"), .x)) %>%
    map2_df(., thread_parameter[[file_suffix]]$val_pos,
            ~str_sub(.x, 1, .y)) %>%
    map2_df(., thread_parameter[[file_suffix]]$par_txt,
            paste0) %>%
    transpose() %>%
    map(., unlist) %>%
    map2(., thread_parameter[[file_suffix]]$file,
         function(x, y, pos){y[pos] <- x
                             return(y)}, par_pos)

  walk2(par_files, file_sel$file, ~ writeLines(.x, thread_path%//%.y))
}

#' Write modified .chm SWAT parameter files due to their specific format
#'
#' @param file_meta Table that provides the parameter file meta data
#' @param thread_parameter Modified model parameters to be written to respective
#'   thread folder
#' @param thread_path Path to the thread folder where model is executed
#'
#' @importFrom dplyr %>% filter select
#' @importFrom purrr map map2 map_df walk2
#' @importFrom readr write_lines
#' @importFrom tibble as_tibble
#'
#' @keywords internal
#'
write_chm <- function(file_meta, thread_parameter, thread_path) {
  file_sel <- filter(file_meta, file_name == "chm")
  table_pos <-  3:8
  line_name <- thread_parameter$chm$file[[1]][table_pos] %>%
    gsub("\\:.*", "", .) %>%
    paste0(., ":")

  par_files <-  map(file_sel$file_code,
                  ~ filter(thread_parameter$chm$value, file_code == .x)) %>%
    map(., ~ select(.x, -file_code, -idx)) %>%
    map(., ~map_df(.x, ~sprintf("%12.2f", .x))) %>%
    # map(., ~t(.x)) %>%
    map(., ~map(.x, ~paste0(.x, collapse = ""))) %>%
    map(., ~unlist(.x)) %>%
    map(., ~paste0(line_name, .x)) %>%
    map2(., thread_parameter$chm$file, function(x, y, pos){y[pos] <- x
                                         return(y)}, table_pos)
  walk2(par_files, file_sel$file, ~ writeLines(.x, thread_path%//%.y))
}

#' Write modified .sol SWAT parameter files due to their specific format
#'
#' @param file_meta Table that provides the parameter file meta data
#' @param thread_parameter Modified model parameters to be written to respective
#'   thread folder
#' @param thread_path Path to the thread folder where model is executed
#'
#' @importFrom dplyr %>% filter select
#' @importFrom purrr map map2 map_df walk2
#' @importFrom readr write_lines
#' @importFrom tibble as_tibble
#'
#' @keywords internal
#'
write_sol <- function(file_meta, thread_parameter, thread_path) {
  file_sel <- filter(file_meta, file_name == "sol")
  par_pos <- 4:6
  table_pos <-  8:21
  par_name <- thread_parameter$sol$file[[1]][par_pos] %>%
    gsub("\\:.*", "", .) %>%
    paste0(., ":")
  table_name <- thread_parameter$sol$file[[1]][table_pos] %>%
    gsub("\\:.*", "", .) %>%
    paste0(., ":")

  par_file <- map(file_sel$file_code,
                    ~ filter(thread_parameter$sol$value, file_code == .x)) %>%
    map(., ~.x[,1:14]) %>%
    map(., ~map_df(.x, ~sprintf("%12.2f", .x))) %>%
    map(., ~map(.x, ~paste0(.x, collapse = ""))) %>%
    map(., ~unlist(.x)) %>%
    map(., ~paste0(table_name, .x)) %>%
    map2(., thread_parameter$sol$file, function(x, y, pos){y[pos] <- x
    return(y)}, table_pos)

  par_file <- map(file_sel$file_code,
      ~ filter(thread_parameter$sol$value, file_code == .x)) %>%
    map(., ~ select(.x, SOL_ZMX, ANION_EXCL, SOL_CRK) %>%
          .[1,] %>%
          as.character(.)) %>%
    map(., ~paste(par_name, .x)) %>%
    map2(., par_file, function(x, y, pos){y[pos] <- x
    return(y)}, par_pos)

  walk2(par_file, file_sel$file, ~ writeLines(.x, thread_path%//%.y))
}

#' Write modified .mgt SWAT parameter files due to their specific format
#'
#' @param file_meta Table that provides the parameter file meta data
#' @param thread_parameter Modified model parameters to be written to respective
#'   thread folder
#' @param thread_path Path to the thread folder where model is executed
#'
#' @importFrom dplyr %>% filter select
#' @importFrom purrr map map2 map2_df transpose walk2
#' @importFrom readr write_lines
#' @importFrom stringr str_sub
#' @importFrom tibble as_tibble
#'
#' @keywords internal
#'
write_mgt <- function(file_meta, thread_parameter, thread_path) {
  file_sel <- filter(file_meta, file_name == "mgt")
  par_pos <- which(is_par(thread_parameter$mgt$file[[1]][1:27]))


  par_files <- thread_parameter[["mgt"]]$value %>%
    select(-idx, -file_code) %>%
    map2_df(., thread_parameter[["mgt"]]$val_pos,
            ~sprintf(paste0("%",.y,"s"), .x)) %>%
    map2_df(., thread_parameter[["mgt"]]$val_pos,
            ~str_sub(.x, 1, .y)) %>%
    map2_df(., thread_parameter[["mgt"]]$par_txt,
            paste0) %>%
    transpose() %>%
    map(., unlist) %>%
    map2(., thread_parameter$mgt$file,
         function(x, y, pos){y[pos] <- x
         return(y[1:30])}, par_pos)

  par_files <- map(file_sel$file_code,
      ~ filter(thread_parameter$mgt$mgt_table, file_code == .x)) %>%
    map(., ~ select(.x, -file_code, -idx)) %>%
    map(., ~apply(.x, 1, format_mgt_line)) %>%
    map(., ~t(.x)) %>%
    map(., ~apply(.x, 1, paste, collapse = " ")) %>%
    map2(par_files, ., ~c(.x, .y))

  walk2(par_files, file_sel$file, ~ writeLines(.x, thread_path%//%.y))
}

#' Format a line in the mgt table for writing in correct format to file
#'
#' @param mgt_line A line in the mgt table to be formatted as character strings
#'
#' @keywords internal
#'
format_mgt_line <- function(mgt_line){
  sprintf(c("%3.0f", "%2.0f", "%8.3f", "%2.0f", "%4.0f", "%3.0f", "%2.0f",
                  "%12.5f", "%6.2f", "%11.5f", "%4.2f", "%6.2f", "%5.2f"),
          mgt_line) %>%
    gsub("NA", "  ", .)
}
