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

write_chm <- function(file_meta, file_suffix,
                      thread_parameter, thread_path) {
  file_sel <- filter(file_meta, file_name == file_suffix)

  map(file_sel$file_code, ~ filter(swat_parameter$chm$value, file_code == .x)) %>%
    map(., ~ select(.x, -file_code, -idx)) %>%
    map(., ~map_df(.x, ~sprintf("%16s", .x))) %>%
    map(., ~t(.x))

  table_pos <-  3:8
}

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
    bind_rows(.) %>%
    mutate(idx = 1:nrow(.))

  return(list(file = files, value = par_table))
}

get_table <- function(file_i, table_pos, col_pos) {
  col_start <- col_pos[1:(length(col_pos)-1)]
  col_end   <- col_pos[2:length(col_pos)] - 1
  file_i[table_pos] %>%
    map_dfc(., ~ split_line(.x, col_start, col_end))
}
