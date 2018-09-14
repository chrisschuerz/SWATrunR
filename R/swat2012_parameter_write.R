write_parameter <- function(file_meta, thread_parameter, thread_path) {
  file_names <- names(thread_parameter)
}

write_par_list <- function(file_meta, file_suffix,
                           thread_parameter, thread_path) {
  file_sel <- filter(file_meta, file_name == file_suffix)
  par_pos <- is_par(thread_parameter[[file_suffix]]$file[[1]])

  par_files <- thread_parameter[[file_suffix]]$value %>%
    select(-idx, -file_code) %>%
    map_df(., ~sprintf("%16s", .x)) %>%
    t(.) %>%
    as_tibble(.) %>%
    map(., ~.x) %>%
    map2(., thread_parameter[[file_suffix]]$file,
         function(x, y, pos){y[pos] <- x
                             return(y)}, par_pos)

  walk2(par_files, file_sel$file, ~ write_lines(.x, thread_path%//%.y))
}

write_chm <- function(file_meta, thread_parameter, thread_path) {
  file_sel <- filter(file_meta, file_name == "chm")
  table_pos <-  3:8
  line_name <- thread_parameter$chm$file[[1]][table_pos] %>%
    gsub("\\:.*", "", .) %>%
    paste0(., ":")

  par_files <-  map(file_sel$file_code,
                  ~ filter(thread_parameter$chm$value, file_code == .x)) %>%
    map(., ~ select(.x, -file_code, -idx)) %>%
    map(., ~map_df(.x, ~sprintf("%12s", .x))) %>%
    # map(., ~t(.x)) %>%
    map(., ~map(.x, ~paste0(.x, collapse = ""))) %>%
    map(., ~unlist(.x)) %>%
    map(., ~paste(line_name, .x)) %>%
    map2(., thread_parameter$chm$file, function(x, y, pos){y[pos] <- x
                                         return(y)}, table_pos)
  walk2(par_files, file_sel$file, ~ write_lines(.x, thread_path%//%.y))
}

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
    map(., ~map_df(.x, ~sprintf("%12s", .x))) %>%
    map(., ~map(.x, ~paste0(.x, collapse = ""))) %>%
    map(., ~unlist(.x)) %>%
    map(., ~paste(table_name, .x)) %>%
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

  walk2(par_file, file_sel$file, ~ write_lines(.x, thread_path%//%.y))
}

write_mgt <- function(file_meta, thread_parameter, thread_path) {
  file_sel <- filter(file_meta, file_name == "sol")
