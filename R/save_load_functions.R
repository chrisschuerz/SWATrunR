#' Save results of model run i in existing sql data base
#'
#' @param save_path Path of the sql data base
#' @param model_output output of the i_run'th simulation as a tibble
#' @param parameter Vector or tibble with parameter sets
#' @param run_index Vector of the indices of runs that are performed
#' @param i_run The i'th run of the SWAT simulation
#'
#' @importFrom dplyr copy_to src_sqlite %>%
#' @importFrom dbplyr src_dbi
#' @importFrom purrr map map2 set_names
#' @importFrom RSQLite dbConnect dbDisconnect SQLite
#' @importFrom tibble tibble
#' @keywords internal
#'
save_run <- function(save_path, model_output, parameter, run_index, i_run, i_thread) {

  if(is.data.frame(parameter$values)) {
    n_digit <- parameter$values %>%
      nrow(.) %>%
      as.character(.) %>%
      nchar(.)
  }
  run_name <- "run"%_%sprintf("%0"%&%n_digit%&%"d", run_index[i_run])
  save_list <- map(model_output, ~.x) %>%
    map(.,  ~tibble(.x) %>% set_names(.,run_name)) %>%
    set_names(names(.)%&%"$$from$$"%&%run_name)

  output_con <- dbConnect(SQLite(), save_path%//%"sim"%_%i_thread%.%"sqlite")
  output_db <- src_dbi(output_con)

  map2(save_list, names(save_list),
       ~copy_to(dest = output_db, df = .x, name = .y, temporary = FALSE))

  dbDisconnect(output_con)
}

#' Set the save path to the sqlite data base file
#'
#' @param project_path Character string. Path of SWAT project
#' @param save_path (optional) character string. save path if different to
#'   project path
#' @param save_dir character string. Name of the sqlite data base directory
#'
#' @keywords internal
#'
set_save_path <- function(project_path, save_path, save_dir) {
  if(is.null(save_path)) save_path <- project_path

  save_path <- save_path%//%save_dir

  if(!dir.exists(save_path)) dir.create(save_path, recursive = TRUE)

  return(save_path)
}

#' Initialize the data base wher model outputs are saved
#'
#' @param save_path Character string. Path of the sql data base
#' @param parameter Parameter set provided for simualtion
#' @param model_setup List with files and variables that define the SWAT model
#'   setup
#'
#' @importFrom dplyr collect copy_to mutate select src_sqlite tbl %>%
#' @importFrom dbplyr src_dbi
#' @importFrom lubridate year month day hour minute second
#' @importFrom RSQLite dbConnect dbDisconnect SQLite
#' @keywords internal
#'
initialize_save_file <- function(save_path, parameter, model_setup) {
  output_con <- dbConnect(SQLite(), save_path%//%"par_dat.sqlite")
  output_db <- src_dbi(output_con)

  table_names <- src_tbls(output_db)

  if("parameter_values"%in% table_names) {
    if(is.null(parameter)) {
      stop("No parameter set provided to current SWAT run."%&&%
           "Parameter set however found in 'save_file'.")
    }
    par_val <- tbl(output_db, "parameter_values") %>% collect(.)
    if(!identical(as.matrix(parameter$values), as.matrix(par_val))) {
      stop("Parameters of current SWAT simulations and the parameters"%&&%
           "saved in 'save_file' differ!")
    }
    par_def <- tbl(output_db, "parameter_definition") %>% collect(.)
    if(!identical(as.matrix(parameter$definition), as.matrix(par_def))) {
      stop("Parameter definition of current SWAT simulation and the"%&&%
           "parameter definition saved in 'save_file' differ!")
    }
  } else {
    if(!is.null(parameter)){
      if(!is.data.frame(parameter$values))
        parameter$values <-  map_dfc(parameter$values, ~.x)

      copy_to(dest = output_db, df = parameter$values,
              name = "parameter_values", temporary = FALSE)

      copy_to(dest = output_db, df = parameter$definition,
              name = "parameter_definition", temporary = FALSE)
    }
  }

  date <- get_date_vector(model_setup) %>%
    convert_date(.)

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

#' Load saved SWAT simulations
#'
#' Load simuation results that were saved in sqlite data bases in one or
#' multiple save folders when running SWAT.
#'
#' @param save_dir Character string or vector of character strings that provide
#'   the path/s to the save folder/s.
#' @param variable Output variables that were saved in the SWAT run and that
#'   should be loaded into R
#' @param run Numeric vector giving the indexes of the simulations that should
#'   be loaded
#' @param add_parameter Logical. If \code{add_parameter = TRUE} the parameter
#'   set for the SWAT runs is added to the loaded data
#' @param add_date Logical. If \code{add_date = TRUE} a date column is added to
#'   the simulation results of each variable
#'
#' @importFrom dplyr filter %>%
#' @importFrom purrr map walk
#' @importFrom RSQLite dbDisconnect
#' @export
#'
load_swat_run <- function(save_dir, variable = NULL, run = NULL,
                          add_parameter = TRUE, add_date = TRUE) {
  save_list <- scan_save_files(save_dir)

  if(is.null(variable)) {variable <- unique(save_list$table_overview$var)}
  if(is.null(run)) {run <- unique(save_list$table_overview$run_num) %>% sort(.)}

  if(add_date){
    date <- convert_date(save_list$date_data[[1]])
  }

  parameter <- list(values = save_list$par_val[[1]],
                    definition = save_list$par_def[[1]])

  run_list <- run %>%
    map(., ~ filter(save_list$table_overview, run_num %in% .x)) %>%
    filter_not_empty(.)

  run_avail <- map_dbl(run_list, ~.x$run_num[1])

  sim_results <- run_list %>%
    map(., ~ filter(., var %in% variable)) %>%
    map(., ~ split(.x, 1:nrow(.x))) %>%
    map(., ~ collect_sim_run(.x, save_list), save_list) %>%
    tidy_results(., parameter, date, add_parameter, add_date, run_avail)

  if(is.list(sim_results)) {
    if(add_parameter) {
      run_load <- map(sim_results$simulation, ~ names(.x))
    } else {
      run_load <- map(sim_results, ~ names(.x))
    }
    run_load <- run_load %>%
      map(., ~.x[ .x != "date"]) %>%
      map(., ~ gsub("run_", "", .x)) %>%
      map(., ~ as.numeric(.x))

    run_in_loaded <- map(run_load, ~ run %in% .x)
    if(any(!map_lgl(run_in_loaded, all))) {
      cat("Here comes more...")
    }
  }


  walk(save_list$par_dat_con, ~ dbDisconnect(.x))
  walk(save_list$sim_con, ~ dbDisconnect(.x))

  return(sim_results)
}

#' Helper function to collect a table from a sqlite date base according to the
#' information given in one line of the table_overview
#'
#' @param sim_i One line of the overview_table providing with the columns
#'   tbl_name (the name of a table in one of the sqlite db's), var (name of the
#'   variable), run_label, run_num (the numeric representation of a run), and
#'   the con_number (providing the number of the data base connection that
#'   stores the results for this variable and run)
#'
#' @importFrom dplyr collect tbl %>%
#' @importFrom purrr set_names
#' @keywords internal
#'
collect_sim_i <- function(sim_i, save_list){
  con <- save_list$sim_db[[sim_i$con_number]]
  tbl <- sim_i$tbl_name
  var_name <- sim_i$var
  tbl(con, tbl) %>%
    collect(.) %>%
    set_names(.,var_name)
}

#' Wrapper for sim_i that collects all variables for a simulation run
#'
#' @param sim_i same as sim_i above but multiple lines where each line is a
#'   variable for the run_i
#'
#' @importFrom dplyr bind_cols %>%
#' @importFrom purrr map
#' @keywords internal
#'
collect_sim_run <- function(sim_run, save_list) {
  map(sim_run, ~ collect_sim_i(.x, save_list), save_list) %>%
    filter_not_empty(.) %>%
    bind_cols(.)
}

#' Filter the elements of a list with tibbles where the tibbles are not empty
#'
#' @param dat_list List of tibbles (data.frames)
#'
#' @importFrom purrr map_lgl
#' @keywords internal
#'
filter_not_empty <- function(dat_list) {
  is_not_empty <- map_lgl(dat_list, ~ nrow(.x) > 0)
  return(dat_list[is_not_empty])
}

#' Retrieve information on saved SWAT runs
#'
#' Scan one or multiple save folders that belong to the same SWAT simulation and
#' get information on the simulation period, simulated variables and used
#' parameter sets.
#'
#' @param save_dir Character string or vector of character strings that provide
#'   the path/s to the save folder/s.
#'
#' @importFrom dplyr bind_cols %>%
#' @importFrom purrr map walk walk2
#' @importFrom RSQLite dbDisconnect
#' @importFrom tibble tibble
#' @export
#'
scan_swat_run <- function(save_dir) {
  save_list <- scan_save_files(save_dir)

  duplicates <- find_duplicate(save_list$table_overview)
  has_duplicates <- length(unlist(duplicates)) > 0
  run_display <- display_runs(save_list$table_overview)
  date_display <- display_date(save_list$date_data)

  name_length <- run_display %>%
    names(.) %>%
    map(., ~nchar(.x)) %>%
    unlist(.) %>%
    max(.) %>%
    paste0("%-", ., "s")

  cat("Simulation period:\n", date_display, "\n")
  cat("\n")
  cat("Simulated variables:\n")
  walk2(names(run_display), run_display,
        ~ cat(sprintf(name_length, .x)%&%":", "runs", .y,"\n"))
  if(has_duplicates) {
    cat("\n")
    cat("Warning! Duplicates found!\n")
    walk2(names(duplicates), duplicates, function(x,y){
      if(length(y) > 0){
        cat("For", sprintf(name_length, x), "runs", paste(y, collapse = ", "),
            "occured more than once.\n")
      }
    })
  }
  cat("\n")
  cat("Parameter set:\n")
  if(!is.null(save_list$par_val[[1]])) {
    print(save_list$par_val[[1]])
    print(save_list$par_def[[1]])
  } else {
    cat("No data set provided in the save files.")
  }

  walk(save_list$par_dat_con, ~ dbDisconnect(.x))
  walk(save_list$sim_con, ~ dbDisconnect(.x))
}

#' Scan the save folders of a SWAT run and return all meta data of this
#' simulation
#'
#' @param save_dir Character string or vector of character strings that provide
#'   the path/s to the save folder/s.
#'
#' @importFrom dplyr bind_rows collect filter mutate src_tbls tbl %>%
#' @importFrom dbplyr src_dbi
#' @importFrom purrr map map2
#' @importFrom RSQLite dbConnect SQLite
#' @importFrom tibble tibble
#' @keywords internal
#'
scan_save_files <- function(save_dir) {
  # Acquire the paths of all '.sqlite' in the provided save folder paths
  sq_file <- save_dir %>%
    map(., ~list.files(path = .x, pattern = ".sqlite$", full.names = TRUE)) %>%
    unlist(.)

  # split the files into parameter/date files and simulation files
  par_dat_file <- sq_file[grepl("par_dat.sqlite$", sq_file)]
  sim_file <- sq_file[!grepl("par_dat.sqlite$", sq_file)]


  par_dat_con <- map(par_dat_file, ~ dbConnect(SQLite(), .x))
  par_dat_db <- map(par_dat_con, ~src_dbi(.x))
  par_available <- map(par_dat_db, ~ "parameter_values" %in% src_tbls(.x)) %>%
    unlist(.) %>%
    any(.)

  if(par_available) {
    par_val <- map(par_dat_con, ~tbl(.x, "parameter_values") %>% collect(.))
    if(!is_identical(par_val)) {
      stop("The parameter sets in the provided save folders differ!")
    }

    par_def <- map(par_dat_con, ~tbl(.x, "parameter_definition") %>% collect(.))
    if(!is_identical(par_def)) {
      stop("The parameter definitions in the provided save folders differ!")
    }
  }else {
    par_val <- NULL
    par_def  <- NULL
  }

  date_data <- map(par_dat_con, ~tbl(.x, "date") %>% collect(.))
  if(!is_identical(date_data)) {
    stop("The dates in the provided save folders differ!")
  }

  sim_con <- map(sim_file, ~ dbConnect(SQLite(), .x))
  sim_db <- map(sim_con, ~src_dbi(.x))

  table_overview <- map(sim_db, ~src_tbls(.x)) %>%
    map(.,     ~tibble(tbl_name = .x,
                       var       = strsplit(tbl_name, "\\$\\$from\\$\\$") %>%
                         map(., ~.x[1]) %>%
                         unlist(.),
                       run_label = strsplit(tbl_name, "\\$\\$from\\$\\$") %>%
                         map(., ~.x[2]) %>%
                         unlist(.),
                       run_num   = run_label %>%
                         gsub("run_", "", .) %>%
                         as.integer(.))) %>%
    map2(., 1:length(.), ~ mutate(.x, con_number = .y)) %>%
    bind_rows(.) %>%
    filter(!is.na(run_num))


  return(list(par_dat_file   = par_dat_file,
              par_dat_con    = par_dat_con,
              par_dat_db     = par_dat_db,
              par_val        = par_val,
              par_def        = par_def,
              date_data      = date_data,
              sim_file       = sim_file,
              sim_con        = sim_con,
              sim_db         = sim_db,
              table_overview = table_overview))
}

# merge_swat_run <- function(save_dir) {
#
# }


#' Check if tables in a list are identical
#'
#' @param tbl_list List of data.frames
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @keywords internal
#'
is_identical <- function(tbl_list) {
  tbl_list %>%
    map2(.,.[1], ~identical(.x,.y)) %>%
    unlist(.) %>%
    all(.)
}

#' Find simulation runs that are duplicated in the provided sqlite data bases
#'
#' @param tbl overview table that provides meta data for all simulation runs for
#'   all variables saved in the data bases
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @keywords internal
#'
find_duplicate <- function(tbl) {
  tbl %>%
    split(., as.factor(.$var)) %>%
    map(., ~table(.x$run_num)) %>%
    map(., ~.x[.x > 1])
}

#' Convert the information on available runs for the simulated variables into
#' strings that are printed
#'
#' @param tbl overview table that provides meta data for all simulation runs for
#'   all variables saved in the data bases
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map map2
#' @keywords internal
#'
display_runs <- function(tbl) {
  runs <- tbl %>%
    split(., as.factor(.$var)) %>%
    map(., ~table(.x$run_num)) %>%
    map(., ~ names(.x) %>% as.numeric(.))

  runs_consistent <- map(runs, ~ diff(.x) %>% .[.!= 1])

  map2(runs, runs_consistent, function(x,y){
    if(length(y) == 0) {
      paste(min(x), max(x), sep = " to ")
    } else {
      paste(c(x[1:10], "..."), collapse = ", ")
    }
  })
}

#' Convert the information on the dates for the simulated variables into strings
#' that are printed
#'
#' @param date_data Tables holding the dates loaded from the sqlite data bases
#'
#' @importFrom dplyr filter %>%
#' @keywords internal
#'
display_date <- function(date_data) {
  date_data[[1]] %>%
    convert_date(.) %>%
    filter(date == min(date) | date == max(date)) %>%
    .$date %>%
    as.character(.) %>%
    paste(., collapse = " to ")
}

#' Convert dates from date format to year/month/day/hour/min/sec columns or vice
#' versa
#'
#' @param date_tbl Table holding either one date column or the splitted date
#'   columns
#'
#' @importFrom dplyr transmute %>%
#' @importFrom lubridate year month day hour minute second ymd_hms
#' @keywords internal
#'
convert_date <- function(date_tbl) {
  if(ncol(date_tbl) == 1){
    date_tbl %>%
      transmute(year  = year(date),
                month = month(date),
                day   = day(date),
                hour  = hour(date),
                min   = minute(date),
                sec   = second(date))
  } else {
    date_tbl %>%
      transmute(date = ymd_hms(year%//%month%//%day%&&%hour%&&%min%&&%sec))
  }
}

#' Do general checkups for a SQLite database that already holds saved data and
#' compare with the current run_swat inputs
#'
#' @param save_path Path to the folder that holds the saved data
#' @param parameter List that provides Parameter set for the simualtion and the
#'   parameter definition table
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map map2
#' @keywords internal
#'
check_saved_data <- function(save_path, parameter, output, run_index) {
  saved_data <- scan_save_files(save_path)

  if(!is.null(saved_data$par_val)) {
    if(!identical(as.matrix(parameter$values),
                  as.matrix(saved_data$par_val[[1]]))) {
      stop("Parameters of current SWAT simulations and the parameters"%&&%
             "saved in 'save_file' differ!")
    }
    if(!identical(as.matrix(parameter$definition),
                  as.matrix(saved_data$par_def[[1]]))) {
      stop("Parameter definition of current SWAT simulation and the"%&&%
             "parameter definition saved in 'save_file' differ!")
    }
  }
  if(nrow(saved_data$table_overview) > 0) {
    out_var_current <- output %>%
      map2(., names(.), ~ paste0(.y, .x$label_ind))
    tbl_ovr <- saved_data$table_overview
    is_out_saved <- map(out_var_current,
                        ~ any(tbl_ovr$run_num[tbl_ovr$var == .x] %in%
                                run_index)) %>%
      unlist(.)

    if(any(is_out_saved)) {
      stop("Completed simulations for defined variables"%&&%
             "and respective run_indices were found in save_file!\n"%&&%
             "Please check with scan_swat_run() or define new 'save_file'" %&&%
             "for the new simulations!")
    }
  }
}
