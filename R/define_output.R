#' Define SWAT output variables
#'
#' Define the SWAT output variables that should be extracted after the SWAT
#' model execution and be returned to R. It is required to use this function to
#' pass the desired outputs with the variable \code{output} in the function calls
#' \code{\link{run_swat2012}} and \code{\link{run_swatplus}}. See
#' the examples how to use the \code{output} definition together with
#' \code{\link{run_swat2012}} or \code{\link{run_swatplus}}. Further, more comprehensive
#' examples are provided on the package's 'Get Started' page in the section
#' '\href{https://chrisschuerz.github.io/SWATplusR/articles/SWATplusR.html#first-swat-model-runs}{First SWAT model runs}.
#'
#' @param file Character string. The SWAT output file to read.
#' (Valid inputs are \code{'rch'}, \code{'sub'}, \code{'hru'}, and \code{'sed'} for
#'   the respective SWAT2012 output files 'output.rch', 'output.sub',
#'   output.hru', and 'output.sed'. For the respective SWAT+ output files see
#'   the available options in the 'print.prt' file of your SWAT+ project).
#' @param variable Character string. Output variable that is extracted from the
#'   respective SWAT output file defined with \code{file}. For a correct
#'   definition of SWAT2012 output variables please use the
#'   variable documented in the
#'   \href{https://swat.tamu.edu/media/69395/ch32_output.pdf}{SWAT Output Data
#'   Documentation}. For SWAT+ output variables please use the header names from
#'   the respective output table, \strong{without the units!} Optionally, the
#'   column number of a variable in the respective output file can be provided.
#'   \strong{CAUTION:} spaces (e.g. in P TOT) must be replaced with underscores
#'   (P_TOT).
#' @param unit Numeric vector. The spatial unit (e.g. the reach, subbasin, or
#'   HRU) defined by the columns 'RCH', 'SUB', 'HRU' in the respective SWAT2012
#'   output file or the 'unit' column in the SWAT+ output file for which the
#'   outputs should be extracted.
#'
#' @importFrom tibble tibble
#' @importFrom stringr str_sub
#' @export
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
define_output <- function(file, variable = NULL, unit = NULL){
  if(is.null(variable)) {
    variable <- NA_character_
  } else {
    variable <- variable %>%
      remove_units_2012(.) %>%
      remove_units_plus(.)
  }

  if (is.null(unit)) {
    unit <- list(NA_integer_)
  } else {
    if (file %in% c('mgtout', paste0('basin_crop_yld_', c('aa', 'yr'))) &
        is.character(unit[1])) {
      unit <- list(unit)
    } else if (is.character(unit[1])){
      stop("'unit' of type 'character' is not allowed for ", file, ' outputs.')
    } else {
      unit <- list(as.integer(unit))
    }
  }

  output <- tibble(file     = file,
                   variable = variable,
                   unit     = unit)

  return(output)
}

#' Check output if is a data.frame and convert in case to named list
#'
#' @param output Output (or list of outputs) defined with \code{define_output()}
#' @param swat_vers SWAT version one of '2012' or 'plus'
#'
#' @importFrom dplyr mutate rename %>%
#' @importFrom purrr map2_df
#' @importFrom stringr str_sub
#'
#' @keywords internal
#'
prepare_output_definition <- function(output, swat_vers, project_path) {
  if (is.data.frame(output)) {
    output <- mutate(output, name = variable, .before = 1)
  } else {
    output <- map2_df(output, names(output), ~ mutate(.x, name = .y, .before = 1))
  }

  has_no_unit <- is.na(output$unit) &
                 !output$file %in% c('mgtout', paste0('basin_crop_yld_', c('aa', 'yr')))
  if(any(has_no_unit)) {
    stop("\nThe following output variables were defined without defining a 'unit':\n",
         paste(output$name[has_no_unit], collapse = ', '))
  }

  is_no_yld_output <- output$file %in% paste0('basin_crop_yld_', c('aa', 'yr')) &
                      !output$variable %in% c('harv_area', 'yld_total', 'yld')

  if(any(is_no_yld_output)) {
    stop("\n Wrong 'variable' defined for 'basin_crop_yld' output.\n",
         "'variable' must be either 'harv_area', 'yld_total', or 'yld'.")
  }

  is_no_mgt_output <- output$file == 'mgtout' &
                      !output$variable %in% c('yld', 'phu', 'bioms', 'wat_strs',
                                              'aer_strs','tmp_strs', 'n_strs',
                                              'p_strs')

  if(any(is_no_mgt_output)) {
    stop("\n Wrong 'variable' defined for 'mgtout' output. ",
         "'variable' must one of:\n 'yld', 'phu', 'bioms', 'wat_strs', ",
         "'aer_strs','tmp_strs', 'n_strs', 'p_strs'.")
  }

  if(swat_vers == "2012") {
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
      c('channel_sdmorph') %>%
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
