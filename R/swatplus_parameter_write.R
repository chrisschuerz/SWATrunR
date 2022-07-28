#' Translate the parameter inputs into a parameter input table and a separarate
#' table providing the file constraints and the filter expressions for the
#' respective parameter
#'
#' @param parameter Model parameters as named vector or tibble
#'
#' @keywords internal
#'
format_swatplus_parameter <- function(parameter) {
  if(!any(names(parameter) %in% c("values", "definition"))) {
    par_constrain <- translate_parameter_constraints(names(parameter), 'plus')
    names(parameter) <- par_constrain$par_name
    if(!is.data.frame(parameter)) parameter <- map_dfc(parameter, ~.x)
    return(list(values = parameter, definition = par_constrain))
  } else {
    return(parameter)
  }
}


#' Update the calibration file structure with the parameter set of the current
#' simulation run_i
#'
#' @param thread_path Path to the current parallel thread 'thread_i'
#' @param parameter Model parameters as named vector or tibble
#' @param calibration Template table structure of the calibration file
#' @param i_run Index of the i_th simulation run
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map2_df map_dbl
#' @importFrom readr write_lines
#' @importFrom stringr str_sub
#'
#' @keywords internal
#'
write_calibration <- function(thread_path, parameter, calibration, run_index,
                              i_run) {
  cal_pos <- which(is.na(calibration$VAL))
  cal_names <- calibration$NAME[cal_pos]

  calibration$VAL[cal_pos] <- parameter$values[run_index[i_run],] %>%
    unlist(.) %>%
    # map_dbl(., ~.x) %>%
    set_names(., parameter$definition$parameter) %>%
    sprintf("%.15f", .) %>%
    str_sub(., 1, 15)

  col_format <- c("%-12s", "%8s", "%16s", "%16s", rep("%8s", ncol(calibration) - 4))

  col_names <- names(calibration) %>%
    sprintf(col_format, .) %>%
    paste(., collapse = "") %>%
    str_remove_all(., 'OBJ\\_[:digit:]') %>%
    str_trim(.)

  calibration <- map2(calibration, col_format, ~sprintf(.y, .x)) %>%
    map_df(., ~ str_replace_all(.x, 'NA', '')) %>%
    apply(., 1, paste, collapse = "") %>%
    c("Number of parameters:", sprintf("%2d",length(cal_pos)), col_names, .) %>%
    str_trim(.)

  write_lines(calibration, thread_path%//%"calibration.cal")
}


#' Check if the names of the defined parameters are available in 'cal_parms.cal'.
#'
#' @param project_path Path to the SWAT+ project
#' @param parameter Model parameter data set
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @importFrom readr read_lines
#'
#' @keywords internal
#'
check_swatplus_parameter <- function(project_path, parameter) {
  if("cal_parms.cal" %in% list.files(project_path)) {
    cal_parms <- read_lines(project_path%//%"cal_parms.cal", skip = 3, lazy = FALSE) %>%
      strsplit(., "\\s+") %>%
      map(., ~ .x[1]) %>%
      unlist(.)
    in_cal_parms <- parameter$definition$parameter %in% cal_parms

    if(any(!in_cal_parms)){
      stop("Parameters"%&&%
           paste(parameter$definition$par_name[!in_cal_parms], collapse = ", ")%&&%
           "not defined in 'cal_parms.cal'")
    }
  } else {
    stop("The file 'cal_parms.cal is missing in SWAT+ project!")
  }
}

#' Read the unit numbers (for hru, aqu, cha, res) and the textures etc for later
#' parameter conditioning.
#'
#' @param project_path Path to the SWAT+ project
#'
#' @importFrom dplyr filter select %>%
#' @importFrom purrr map
#' @importFrom readr read_lines
#'
#' @keywords internal
#'
read_unit_conditions <- function(project_path, parameter) {
  if('unit' %in% names(parameter$definition)) {
    unit_cond <- parameter$definition %>%
      select(file_name, unit) %>%
      filter(!is.na(unit)) %>%
      .$file_name %>%
      unique(.)
  } else {
    unit_cond <- NULL
  }

  units <- list()
  if ('hru' %in% unit_cond) {
    units$hru <- get_tbl_column(project_path%//%'hru-data.hru', 'id')
  }
  if ('sol' %in% unit_cond) {
    units$sol <- get_tbl_column(project_path%//%'hru-data.hru', 'id')
  }
  if ('cha' %in% unit_cond) {
    cha_file <- list.files(project_path, pattern = 'channel.*\\.cha')[1] # maybe removed when clear which the final channel file is.
    units$cha <- get_tbl_column(project_path%//%cha_file, 'id')
  }
  if ('res' %in% unit_cond) {
    units$res <- get_tbl_column(project_path%//%'reservoir.res', 'id')
  }
  if ('aqu' %in% unit_cond) {
    units$aqu <- get_tbl_column(project_path%//%'aquifer.aqu', 'id')
  }
  #swq Not yet considered,
  # Remaining two object types hlt and pst also not yet implemented.
  conds <- list(hsg = LETTERS[1:4],
                texture = get_sol_texture(project_path%//%'soils.sol'),
                plant   = get_tbl_column(project_path%//%'plants.plt', 'name') %>% unique(),
                landuse = get_tbl_column(project_path%//%'landuse.lum', 'plnt_com') %>% unique()
  )
  return(list(units = units, conds = conds))
}


#' Acquire the object indices of from the respective object file.
#'
#' @param file Path to the object file
#'
#' @importFrom dplyr %>%
#' @importFrom readr read_table2 cols col_character col_double
#'
#' @keywords internal
#'
get_tbl_column <- function(file, col_i) {
  suppressWarnings(read_table2(file, skip = 1,
              col_types = cols(id = col_double(),
                               .default = col_character()))) %>%
    .[[col_i]]
}

#' Acquire the object indices of from the respective object file.
#'
#' @param file Path to the object file
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map_chr
#' @importFrom readr read_lines
#' @importFrom stringr str_split str_subset str_trim
#'
#' @keywords internal
#'
get_sol_texture <- function(file) {
  read_lines(file, lazy = FALSE) %>%
    .[-c(1,2)] %>%
    str_subset(.,'^[:graph:]') %>%
    str_trim(.) %>%
    str_split(., '[:space:]+') %>%
    map_chr(., ~.x[7])
}

