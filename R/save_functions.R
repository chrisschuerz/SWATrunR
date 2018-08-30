#' Save results of model run i in existing sql data base
#'
#' @param save_path Path of the sql data base
#' @param model_output output of the i_run'th simulation as a tibble
#' @param parameter Vector or tibble with parameter sets
#' @param i_run The i'th run of the SWAT simulation
#'
#' @importFrom dplyr copy_to src_sqlite %>%
#' @importFrom dbplyr src_dbi
#' @importFrom pasta %&% %_% %.%
#' @importFrom purrr map map2 set_names
#' @importFrom RSQLite dbConnect dbDisconnect SQLite
#' @importFrom tibble tibble
#' @keywords internal
#'
save_run <- function(save_path, model_output, parameter, i_run, i_thread) {
  if(is.data.frame(parameter)) {
    n_digit <- parameter %>%
      nrow(.) %>%
      as.character(.) %>%
      nchar(.)
  }
  run_name <- "run"%_%sprintf("%0"%&%n_digit%&%"d", i_run)
  save_list <- map(model_output, ~.x) %>%
    map(.,  ~tibble(.x) %>% set_names(.,run_name)) %>%
    set_names(names(.)%&%"$$from$$"%&%run_name)

  output_con <- dbConnect(SQLite(), save_path%//%"sim"%_%i_thread%.%"sqlite")
  output_db <- src_dbi(output_con)

  map2(save_list, names(save_list),
       ~copy_to(dest = output_db, df = .x, name = .y, temporary = F))

  dbDisconnect(output_con)
}

#' Set the save path to the sqlite data base file
#'
#' @param project_path Character string. Path of SWAT project
#' @param save_path (optional) character string. save path if different to
#'   project path
#' @param save_dir character string. Name of the sqlite data base directory
#'
#' @importFrom pasta %//%
#' @keywords internal
#'
set_save_path <- function(project_path, save_path, save_dir) {
  if(is.null(save_path)) save_path <- project_path
  save_path <- save_path%//%save_dir
  dir.create(save_path, recursive = TRUE)
  return(save_path)
}

#' Initialize the data base wher model outputs are saved
#'
#' @param save_path Character string. Path of the sql data base
#' @param parameter Parameter set provided for simualtion
#' @param file_cio The modified file.cio required to calculate the date
#'
#' @importFrom dplyr collect copy_to mutate select src_sqlite tbl %>%
#' @importFrom dbplyr src_dbi
#' @importFrom lubridate year month day hour minute second
#' @importFrom pasta %.% %//%
#' @importFrom RSQLite dbConnect dbDisconnect SQLite
#' @keywords internal
#'
initialize_save_file <- function(save_path, parameter, file_cio) {
  output_con <- dbConnect(SQLite(), save_path%//%"par_dat.sqlite")
  output_db <- src_dbi(output_con)

  table_names <- src_tbls(output_db)

  if("parameter"%in% table_names) {
    par_db <- tbl(output_db, "parameter") %>% collect(.)
    if(!identical(as.matrix(parameter), as.matrix(par_db))) {
      stop("Parameters of current SWAT simulations and the parameters"%&&%
           "saved in 'save_file' differ!")
    }
  } else {
    if(!is.null(parameter)){
      if(!is.data.frame(parameter)) parameter <-  map_dfc(parameter, ~.x)

      copy_to(dest = output_db, df = parameter,
              name = "parameter", temporary = FALSE)
    }
  }

  date <- read_date(file_cio) %>%
    mutate(year  = year(date),
           month = month(date),
           day   = day(date),
           hour  = hour(date),
           min   = minute(date),
           sec   = second(date)) %>%
    select(-date)

  if("date"%in% table_names) {
    date_db <- tbl(output_db, "date") %>% collect(.)
    if(!identical(as.matrix(date), as.matrix(date_db))) {
      stop("Date of current SWAT simulations and the date"%&&%
           "saved in 'save_file' differ!")
    }
  } else {
    copy_to(dest = output_db, df = date,
            name = "date", temporary = FALSE)
  }
  dbDisconnect(output_con)
}
