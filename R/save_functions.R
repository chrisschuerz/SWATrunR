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

set_save_path <- function(project_path, save_path, save_file) {

}

initialize_save_file <- function(variables) {

}
