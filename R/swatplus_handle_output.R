# Functions for reading SWAT+ outputs

#' Read SWAT+ output files
#'
#' @param output Output defined to read from the SWAT model results
#' @param thread_path Path to respective thread where SWAT was executed
#' @param add_date Logical wheter to add date column
#'
#' @importFrom dplyr arrange filter group_by group_split mutate relocate select %>%
#' @importFrom lubridate ymd
#' @importFrom purrr map map2 map_chr set_names
#' @importFrom readr fwf_positions read_fwf
#' @keywords internal
#'
read_swatplus_output <- function(output, thread_path, add_date, split_units) {

  if(add_date){
    date_cols <- c('yr', 'mon', 'day')
  } else {
    date_cols <- c()
  }

  # Split into time series outputs and other outputs (yields, FDC)
  output_ts <- filter(output, !file %in% c('basin_crop_yld', 'fdcout', 'mgtout'))
  output_yld <- filter(output,  file %in% c('basin_crop_yld'))
  output_mgt <- filter(output,  file %in% c('mgtout'))
  output_fdc <- filter(output,  file %in% c('fdcout'))

  if(nrow(output_ts) > 0) {
    output_ts <- output_ts %>%
      group_by(file_full) %>%
      group_split()

    unit_names <- output_ts %>%
      map(., ~ fread(thread_path%//%.x$file_full[1], skip = 2, nrows = 1, header = F)) %>%
      map(., ~ unlist(.x) %>% unname(.))

    col_names <- output_ts %>%
      map(., ~ fread(thread_path%//%.x$file_full[1], skip = 1, nrows = 1, header = F)) %>%
      map(., ~ unlist(.x) %>% unname(.)) %>%
      map2(., unit_names, ~ replace_colname_na(.x, .y)) %>%
      map(., ~ .x[!is.na(.x)]) %>%
      map(., ~add_suffix_to_duplicate(.x))

    ## Read all output files, assign column names and assign output file names
    out_tables_ts <- map2(output_ts, col_names, ~ read_output_i(.x, .y, thread_path, date_cols))

    if(add_date) {
      out_tables_ts <- out_tables_ts %>%
        map(., ~ mutate(.x, date = ymd(paste(yr,mon,day, sep = '-')), .before = 1)) %>%
        map(., ~ select(.x, -yr, -mon, -day))
    }

    out_tables_ts <- out_tables_ts %>%
        map(., ~ add_id(.x)) %>%
        map2(., output_ts, ~mutate_output_i(.x, .y, split_units))
  } else {
    out_tables_ts <- NULL
  }

  if (nrow(output_yld) > 0) {
    output_yld <- output_yld %>%
      group_by(file_full) %>%
      group_split()

    out_tables_yld <- map(output_yld, ~ read_basin_yld(.x, thread_path))
  } else {
    out_tables_yld <- NULL
  }

  if (nrow(output_mgt) > 0) {
    out_tables_mgt <- list(read_mgtout(output_mgt, thread_path))
  } else {
    out_tables_mgt <- NULL
  }

  if (nrow(output_fdc) > 0) {
    output_fdc <- list(output_fdc)
    out_tables_fdc <- list(read_fdcout(output_fdc, thread_path))
  } else {
    out_tables_fdc <- NULL
  }

  out_tables <- c(out_tables_ts, out_tables_yld, out_tables_mgt, out_tables_fdc)

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
  fread(thread_path%//%output_i$file_full[1], skip = 3) %>%
    as_tibble(.) %>%
    .[,1:length(col_names_i)] %>%
    set_names(col_names_i) %>%
    select(., all_of(c(date_cols, 'unit', output_i$variable))) %>%
    filter(unit %in% (output_i$unit %>% unlist(.) %>% unique(.)))
}

#' Read basin yield output tables.
#'
#' @param output_i i_th part from the output table which defines what to
#' @param thread_path String path to the thread where to read the output file
#'   read from the SWAT model results
#'
#' @importFrom data.table fread
#' @importFrom dplyr select %>%
#' @importFrom purrr set_names
#' @importFrom tibble as_tibble
#' @importFrom tidyselect all_of
#' @keywords internal
#'
read_basin_yld <- function(output_i, thread_path) {
  yld_header <- c('year', 'no', 'plant_name', 'harv_area', 'yld_total', 'yld')
  fread(thread_path%//%output_i$file_full[1], skip = 2, header = FALSE) %>%
    set_names(., yld_header) %>%
    as_tibble(.) %>%
    select(., year, plant_name, all_of(output_i$variable)) %>%
    set_names(c('year', 'plant_name', output_i$name))
}

#' Read and process SWAT+ management outputs.
#'
#' @param output_i i_th part from the output table which defines what to
#' @param thread_path String path to the thread where to read the output file
#'
#' @importFrom dplyr across arrange bind_rows filter group_by left_join select summarise %>%
#' @importFrom purrr map map_df set_names
#' @importFrom readr read_lines
#' @importFrom stringr str_trim str_split
#' @importFrom tibble as_tibble
#' @importFrom tidyselect all_of everything
#'
#' @keywords internal
#'
read_mgtout <- function(output_i, thread_path) {
  file_path <- paste0(thread_path, '/mgt_out.txt')

  mgt <- read_lines(file_path, skip = 3, lazy = FALSE) %>%
    unlist() %>%
    str_trim(.) %>%
    str_split(., '\t[:space:]+|[:space:]+') %>%
    map(., ~ .x[1:21]) %>%
    unlist() %>%
    matrix(., nrow = 21) %>%
    t() %>%
    as_tibble(., .name_repair = 'minimal') %>%
    set_names(., c('hru', 'year', 'mon', 'day', 'plant_name', 'operation',
                   'phubase', 'phu', 'soil_water', 'bioms',
                   'surf_rsd', 'soil_no3', 'soil_solp', 'yld',
                   'p_strs', 'n_strs', 'tmp_strs', 'wat_strs', 'aer_strs',
                   'v6', 'v7')) %>%
    filter(operation == 'HARVEST') %>%
    select(hru, year, plant_name, all_of(output_i$variable)) %>%
    set_names(c('hru', 'year', 'plant_name', output_i$name))
  mgt[,1:2] <- map_df(mgt[,1:2], as.integer)
  mgt[,4:ncol(mgt)] <- map_df(mgt[,4:ncol(mgt)], as.numeric)

  unit <- unique(unlist(output_i$unit))

  if(length(unit) > 0) {
    if(is.numeric(unit[1])) {
      mgt <- filter(mgt, hru %in% unit)
    } else {
      mgt <- filter(mgt, plant_name %in% unit)
    }
  }

  mgt <- mutate(mgt, label = paste(mgt$hru, mgt$year, mgt$plant_name, sep = '_'))
  multi_harv <- table(mgt$label)
  mgt <- select(mgt, -label)

  if(any(multi_harv > 1)) {
    mgt_multi  <- filter(mgt, label %in% names(multi_harv)[multi_harv > 1])
    mgt_single <- mgt %>%
      filter(., !label %in% names(multi_harv)[multi_harv > 1]) %>%
      select(-label)

    if('yld' %in% output_i$variable & length(unique(output_i$variable)) > 1) {
      mgt_multi_yld <- mgt_multi %>%
        select(label, yld) %>%
        group_by(label) %>%
        summarise(., yld = sum(yld), .groups = 'drop')

      mgt_multi_other <- mgt_multi %>%
        select(-yld, -hru, -year, -plant_name) %>%
        group_by(label) %>%
        summarise(., across(everything(),  max), .groups = 'drop')

      mgt_multi <- mgt_multi %>%
        select(hru, year, plant_name, label) %>%
        left_join(., mgt_multi_yld, by = 'label') %>%
        left_join(., mgt_multi_other, by = 'label') %>%
        select(-label)

      mgt_multi <- mgt_multi[names(mgt_single)]
      mgt <- bind_rows(mgt_single, mgt_multi)
    } else {
      agg_fun <- ifelse('yld' %in% output_i$variable, sum, max)
      mgt_multi <- mgt_multi %>%
        select(-label) %>%
        group_by(hru, year, plant_name) %>%
        summarise(., across(everything(),  agg_fun), .groups = 'drop')

      mgt_multi <- mgt_multi[names(mgt_single)]
      mgt <- bind_rows(mgt_single, mgt_multi)
    }
  }
  mgt <- arrange(mgt, hru, year, plant_name)

  return(mgt)
}

#' Reading the FDC output table.
#'
#' @param output_i i_th part from the output table which defines what to
#'   read from the SWAT model results
#' @param thread_path String path to the thread where to read the output file
#'
#' @importFrom data.table fread
#' @importFrom dplyr filter mutate rename select %>%
#' @importFrom purrr set_names
#' @importFrom stringr str_remove
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tidyselect all_of
#' @keywords internal
#'
read_fdcout <- function(output_i, thread_path) {
  fread(thread_path%//%output_i$file_full[1], skip = 1) %>%
    as_tibble(.) %>%
    rename(unit = props) %>%
    filter(unit %in% output_i$unit[[1]]) %>%
    select(-ob_typ, -area_ha, -mean) %>% # mean removed as all zero in outputs
    pivot_longer(., cols = - unit, names_to = 'p', values_to = output_i$name) %>%
    mutate(., unit = ifelse(length(output_i$unit[[1]]) > 1, paste0('_', unit), '')) %>%
    mutate(p = ifelse(p == 'max', 'p0', p),
           p = ifelse(p == 'min', 'p100', p),
           p = str_remove(p, 'p') %>% as.numeric(.)) %>%
    pivot_wider(., id_cols = p, names_from = unit, names_glue = "{.value}{unit}", values_from = 3)
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
mutate_output_i <- function(out_tbl_i, output_i, split_units) {

  tbl_list <- map(output_i$variable, ~ select(out_tbl_i, id, unit, .x)) %>%
    map(., ~ set_names(.x, c('id', 'unit', 'variable')))

  if(split_units) {
    is_multi_unit <- map_lgl(output_i$unit, ~ length(.x) > 1)

    tbl_mutate <- tbl_list %>%
      map2(., output_i$unit, ~ filter(.x, unit %in% .y)) %>%
      map2(., output_i$name, ~ set_names(.x, c('id', 'unit', .y))) %>%
      map(., ~ mutate(.x, unit = paste0('_', unit))) %>%
      map2(., is_multi_unit, ~ mutate(.x, unit = ifelse(rep(.y, nrow(.x)), unit, ''))) %>%
      map(.,  ~ pivot_wider(.x, id_cols = id, names_from = unit, names_glue = "{.value}{unit}", values_from = 3)) %>%
      map(., ~ select(.x, -id)) %>%
      bind_cols()

    if('date' %in% names(out_tbl_i)) {
      date_col <- out_tbl_i %>%
        filter(., unit == unique(out_tbl_i$unit)[1]) %>%
        select(date)

      tbl_mutate <- bind_cols(date_col, tbl_mutate)
    }
  } else {
    tbl_mutate <- tbl_list %>%
      map2(., output_i$unit, ~ mutate(.x, variable = ifelse(unit %in% .y, variable, NA))) %>%
      map(., ~ select(.x, variable)) %>%
      map2(., output_i$name, ~ set_names(.x, .y)) %>%
      bind_cols(out_tbl_i[c('unit', 'date')], .)

    all_na <- apply(tbl_mutate[,3:ncol(tbl_mutate)], 1, is.na) %>%
      apply(., 2, all)

    tbl_mutate <- tbl_mutate[!all_na, ]
  }

  return(tbl_mutate)
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
