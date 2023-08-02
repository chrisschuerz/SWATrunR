#' Define SWAT output variables
#'
#' @description
#'
#' Define the SWAT+ or SWAT2012 output variables which are returned for a SWAT
#' model run with `run_swatplus()` or `run_swat2012()`. The defined outputs are
#' passed with the input argument `output` in either one of the run functions.
#'
#' @param file The SWAT output file to read.
#'
#'   For SWAT+ simulations with `run_swatplus()` all object files can be used
#'   which are defined in the input file 'print.prt'. All file definitions
#'   require to add the time interval extension in which the outputs should be
#'   returned. The options for the time interval are `'day'` for daily , `'mon'`
#'   for monthly, `'yr'` for yearly, and `'aa'` for average annual outputs.
#'   Additionally, the outputs `'fdcout'` for flow duration curve outputs,
#'   `'mgtout'` for management outputs, or `'basin_crop_yld_<yr or aa>` for
#'   basin crop yields can be defined.
#'
#'   For SWAT2012 simulations with `run_swat2012()` valid inputs are
#'   \code{'rch'}, \code{'sub'}, \code{'hru'}, and \code{'sed'} for the
#'   respective SWAT2012 output files 'output.rch', 'output.sub', output.hru',
#'   and 'output.sed'.
#'
#' @param variable Output variable that is extracted from the defined SWAT
#'   output `file`.
#'
#'   A `variable` must be defined with the name which the variable has in the
#'   respective output table. The variable names are case sensitive (SWAT2012
#'   variables e.g. are usually all caps). In the `variable` definition units
#'   (which are part of the names in SWAT2012 output files) should be excluded
#'   from the name. Spaces in variable names must be replaced by an underscore
#'   (e.g. P TOT is defined as `'P_TOT'`).
#'
#'   Some SWAT+ output files have specific output variable names. For `'mgtout'`
#'   outputs the variables `'yld'`, `'phu'`, `'bioms'`, `'wat_strs'`,
#'   ,`'aer_strs'`,`'tmp_strs'`, `'n_strs'`, and `'p_strs'` can be returned. For
#'   `'basin_crop_yld_*` outputs the variables `'harv_area'`, `'yld_total'`, and
#'   `'yld'` can be returned.
#'
#' @param unit The spatial unit for which outputs are returned.
#'
#'   In SWAT+ output files spatial units are in most cases defined with the
#'   'unit' column and are e.g. the HRU IDs in `'hru_*'` output files, or
#'   channel IDs e.g. in `'channel_sd'` or `'channel_sdmorph_*` output files
#'   (same applies to all other object types).
#'
#'   In SWAT2012 output files `unit` is the  the reach, subbasin, or HRU ID
#'   defined by the columns 'RCH', 'SUB', or 'HRU' in the respective SWAT2012
#'   output file.
#'
#' @returns A table with the columns 'file', 'variable', 'unit', and 'label'.
#'
#' @importFrom tibble tibble
#' @importFrom stringr str_sub
#'
#' @export
#'
#' @examples
#' # A single variable can be defined as follows (e.g. "FLOW_OUT" for
#' # the reaches 1 and 5):
#'
#' out_flow <- define_output(file = "rch",
#'                           variable = "FLOW_OUT",
#'                           unit = c(1,5))
#' # In this case the the variable name of the returned output is then
#' # the same as defined with 'variable', here "FLOW_OUT"
#'
#' # If a custom variable name is preferred for the returned output,
#' # the output must be defined as named list:
#'
#' out_flow <- list(discharge = define_output(file = "rch",
#'                                            variable = "FLOW_OUT",
#'                                            unit = c(1,5)))
#'
#'
#' # Define the discharge for the RCH units 1 to 5 and the
#' # nitrate-nitrogen load and ET for the unit 5:
#'
#' out_def <- list(flow = define_output(file = "rch",
#'                                      variable = "FLOW_OUT",
#'                                      unit = 1:5),
#'                 no3  = define_output(file = "rch",
#'                                      variable = "NO3_OUT",
#'                                      unit = 5),
#'                 et_a = define_output(file = "sub",
#'                                      variable = "ET",
#'                                      unit = 5))
#'
define_output <- function(file, variable = NULL, unit = NULL, label = NULL){
  stopifnot(is.character(file))

  if(is.null(variable) & file != 'fdcout') {
    stop("'variable' must be defined for '", file, "' outputs.")
  } else if (is.null(variable) & file == 'fdcout') {
    variable <- NA_character_
  } else if (length(variable) > 1) {
    stop("'variable' must be single character string. Only one 'variable' per ",
         "output definition can be defined")
  } else if (!is.character(variable)) {
    stop("'variable' must be a character string.")
  } else {
    variable <- variable %>%
      remove_units_2012(.) %>%
      remove_units_plus(.)
  }

  # These files must not have a unit
  file_can_have_no_unit <- c('mgtout', paste0('basin_crop_yld_', c('aa', 'yr')))

  if(is.null(unit) & !file %in% file_can_have_no_unit) {
    stop("'unit' must be defined for '", file, "' output variables.")
  } else if (is.null(unit) & file %in% file_can_have_no_unit) {
    unit <- list(NA_integer_)
  } else if (!(is.numeric(unit) | is.null(unit))) {
    stop("'unit' must be a numeric value/vector.")
  } else {
    unit <- list(as.integer(unit))
  }

  file_can_have_label <- c('mgtout', paste0('basin_crop_yld_', c('aa', 'yr')))

  if (!is.null(label) & (file %in% file_can_have_label |
                         grepl('_pest_', file))) {
    label <- list(label)
  } else if (!is.null(label)) {
    stop("'label' can only be defined for 'mgtout', 'basin_crop_yld_*', or ",
         "'*_pest_*' output files.")
  } else if (!(is.character(label) | is.null(label))) {
    stop("'label' must be a character string or a vector of character strings.")
  } else {
    label <- list(NA_character_)
  }

  is_no_yld_output <- file %in% paste0('basin_crop_yld_', c('aa', 'yr')) &
    !variable %in% c('harv_area', 'yld_total', 'yld')

  if(is_no_yld_output) {
    stop("'variable = ", variable, "' is no valid input for '",
         file ,"'.\n",
         "'variable' can only be 'harv_area', 'yld_total', or 'yld'.")
  }

  is_no_mgt_output <- file == 'mgtout' &
    !variable %in% c('yld', 'phu', 'bioms', 'wat_strs',
                            'aer_strs','tmp_strs', 'n_strs',
                            'p_strs')

  if(is_no_mgt_output) {
    stop("'variable = ", variable, "' is no valid input for '",
          file ,"'.\n",
         "'variable' must one of:\n 'yld', 'phu', 'bioms', 'wat_strs', ",
         "'aer_strs','tmp_strs', 'n_strs', 'p_strs'.")
  }

  output <- tibble(file     = file,
                   variable = variable,
                   unit     = unit,
                   label    = label)

  return(output)
}

#' Check output if is a data.frame and convert in case to named list
#'
#' @param output Output (or list of outputs) defined with \code{define_output()}
#' @param swat_version SWAT version one of '2012' or 'plus'.
#' @param project_path Path to the SWAT project folder on the hard drive
#'   (i.e. txtinout folder)
#'
#' @returns A tibble with:
#'   - `name` User defined variable name
#'   - `file_full` Full output file name
#'   - `file` File name without file extension
#'   - `time_interval` output variable time interval
#'   - `variable` SWAT output variable written as in output file
#'   - `unit` vector of units for which variable is extracted
#'   - `label` Specific plant or pesticide name for which output is extracted
#'
#' @importFrom dplyr mutate rename %>%
#' @importFrom purrr map2_df
#' @importFrom stringr str_sub
#'
#' @keywords internal
#'
prepare_output_definition <- function(output, swat_version, project_path) {
  if (is.data.frame(output)) {
    output <- mutate(output, name = variable, .before = 1)
  } else {
    output <- map2_df(output, names(output), ~ mutate(.x, name = .y, .before = 1))
  }

  if(swat_version == "2012") {
    output <- output %>%
      mutate(file = str_sub(file, (nchar(file) - 2), nchar(file)),
             file = paste0('output.', file))

    is_2012_outfile <- output$file %in% paste0('output.',
                                               c("rch", "sub", "hru", "sed", "rsv"))
    if(any(!is_2012_outfile)) {
      wrong_outputs <- output[!is_2012_outfile,]
      name_length <- max(nchar(c('name',wrong_outputs$name)))
      file_length <- max(nchar(c('output_file', wrong_outputs$file)))
      names <- sprintf(paste0('%', name_length, 's'), c('name', wrong_outputs$name))
      files <- sprintf(paste0('%', file_length, 's'), c('output_file', wrong_outputs$file))
      lines_print <- map2_chr(names, files, ~ paste(.x , .y, '\n', collapse = '   '))

      stop('\nThe output file definitions for the following variables are not supported:\n\n',
           lines_print)
    }
  } else {
    print_prt <- read_lines(paste0(project_path, '/print.prt'), lazy = FALSE)
    out_names <- print_prt[11:length(print_prt)] %>%
      str_trim(.) %>%
      str_split(., '[:space:]+', simplify = TRUE) %>%
      .[,1] %>%
      c('channel_sdmorph', 'aquifer_pest', 'basin_ls_pest', 'basin_res_pest',
        'channel_pest', 'hru_pest', 'reservoir_pest')

    out_names <- out_names[out_names != 'pest'] %>%
      rep(., each = 4) %>%
      paste0(., c('_aa', '_yr', '_mon', '_day')) %>%
      c('fdcout', 'mgtout', paste0('basin_crop_yld_', c('aa', 'yr')))

    is_plus_outfile <- output$file %in% out_names

    if(any(!is_plus_outfile)) {
      wrong_outputs <- output[!is_plus_outfile,]
      name_length <- max(nchar(c('name',wrong_outputs$name)))
      file_length <- max(nchar(c('output_file', wrong_outputs$file)))
      names <- sprintf(paste0('%', name_length, 's'), c('name', wrong_outputs$name))
      files <- sprintf(paste0('%', file_length, 's'), c('output_file', wrong_outputs$file))
      lines_print <- map2_chr(names, files, ~ paste(.x , .y, '\n', collapse = '   '))

      stop('\nThe output file definitions for the following variables are not supported:\n\n',
           lines_print, '\n',
           "Please define 'file' as given in 'print.prt' together with the time interval \n",
           "(adding one of '_aa', '_yr', '_mon', '_day' to the file name).")
    }

    output <- output %>%
      mutate(time_interval = str_extract(file, '[^_]+$'), .after = file) %>%
      mutate(file = str_remove(file, '_[^_]+$')) %>%
      mutate(file_full = paste0(file, '_', time_interval, '.txt'), .before = file) %>%
      mutate(file_full = ifelse(file == 'mgtout', 'mgt_out.txt', file_full)) %>%
      mutate(file_full = ifelse(file == 'fdcout', 'flow_duration_curve.out', file_full))
  }

  return(output)
}



#' Filter function to extract only relevant month entries in SWAT2012 outputs
#'
#' @param mon Month column from output table as vector
#' @importFrom purrr map_dbl
#'
#' @keywords internal
#'
filter_mon <- function(mon) {
  n_group <- which(diff(mon) != 0)[1]
  n_chunk <- length(mon)/n_group
  grp_var <- rep(1:n_chunk, each = n_group) %>% as.factor(.)
  mon_split <- split(mon, grp_var)

  mon_uni <- map_dbl(1:n_chunk, ~mon_split[[.x]][1])
  jumps <- which(diff(mon_uni) != 1) + 1

  chunk_exclude <- jumps[mon_uni[jumps] != 1]

  keep_uni <- rep(TRUE, n_chunk)
  keep_uni[chunk_exclude] <- FALSE

  keep_mon <- rep(keep_uni, each = n_group)

  return(keep_mon)
}
