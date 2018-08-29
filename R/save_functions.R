#' Save results of model run i in existing sql data base
#'
#' @param save_path Path of the sql data base
#' @param model_output output of the i_run'th simulation as a tibble
#' @param parameter Vector or tibble with parameter sets
#' @param i_run The i'th run of the SWAT simulation
#'
#' @importFrom dplyr copy_to src_sqlite %>%
#' @importFrom pasta %&%
#' @importFrom purrr map map2 set_names
#' @importFrom tibble tibble
#' @keywords internal
#'
save_run <- function(save_path, model_output, parameter, i_run) {
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

  output_db <- src_sqlite(save_path)

  map2(save_list, names(save_list),
       ~copy_to(dest = output_db, df = .x, name = .y, temporary = F))
}

#' Set the save path to the sqlite data base file
#'
#' @param project_path Character string. Path of SWAT project
#' @param save_path (optional) character string. save path if different to
#'   project path
#' @param save_file character string. Name of the sqlite data base
#'
#' @importFrom pasta %.% %//%
#' @keywords internal
#'
set_save_path <- function(project_path, save_path, save_file) {
  if(!grepl("\\.sqlite$", save_file)) save_file <- save_file%.%"sqlite"
  if(is.null(save_path)) save_path <- project_path

  return(save_path%//%save_file)
}

#' Initialize the data base wher model outputs are saved
#'
#' @param save_path Character string. Path of the sql data base
#' @param parameter Parameter set provided for simualtion
#' @param file_cio The modified file.cio required to calculate the date
#'
#' @importFrom dplyr collect copy_to mutate select src_sqlite tbl %>%
#' @importFrom lubridate year month day hour minute second
#' @importFrom pasta %.% %//%
#' @keywords internal
#'
initialize_save_file <- function(save_path, parameter, file_cio) {
  if(file.exists(save_path)){
    output_db <- src_sqlite(save_path)
  } else {
    output_db <- src_sqlite(save_path, create = TRUE)
  }

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
}
