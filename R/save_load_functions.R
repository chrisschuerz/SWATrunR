#' Save results of model run i in existing sql data base
#'
#' @param save_path Path of the sql data base
#' @param model_output output of the i_run'th simulation as a tibble
#' @param parameter Vector or tibble with parameter sets
#' @param run_index Vector of the indices of runs that are performed
#' @param i_run The i'th run of the SWAT simulation
#'
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom dplyr mutate %>%
#' @importFrom purrr map map2 set_names
#' @importFrom RSQLite SQLite
#' @keywords internal
#'
save_run <- function(save_path, model_output, parameter, run_index, i_run, i_thread) {
  n_digit <- get_digit(parameter$values)
  run_name <- "run"%_%sprintf("%0"%&%n_digit%&%"d", run_index[i_run])

  # Convert date to numeric if date column is in table
  if('date' %in% colnames(model_output)) {
    model_output <- mutate(model_output, date = as.integer(date))
  }
  # Splitting output table to 2000 column pieces due to column number limit
  # of SQLite
  #
  ncol_max  <- 2000
  col_split <- c(0:floor(ncol(model_output)/ncol_max)*ncol_max,
                 ncol(model_output))

  out_split <- map2(col_split[-length(col_split)] + 1, col_split[-1],
                    ~ model_output[,.x:.y]) %>%
    set_names(run_name%_%1:(length(col_split) - 1))

  output_db <- dbConnect(SQLite(), save_path%//%i_thread%.%"sqlite")

  map2(out_split, names(out_split), ~dbWriteTable(output_db, .y, .x))

  dbDisconnect(output_db)
}

#' Save error report of model run i in error_log sql data base
#'
#' @param save_path Path of the sql data base
#' @param model_output output of the i_run'th simulation as a tibble
#' @param parameter Vector or tibble with parameter sets
#' @param run_index Vector of the indices of runs that are performed
#' @param i_run The i'th run of the SWAT simulation
#'
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbWriteTable
#' @importFrom purrr map map2 set_names
#' @importFrom RSQLite SQLite
#' @importFrom tibble tibble
#' @keywords internal
#'
save_error_log <- function(save_path, model_output, parameter, run_index, i_run) {
  n_digit <- get_digit(parameter$values)
  run_name <- "run"%_%sprintf("%0"%&%n_digit%&%"d", run_index[i_run])

  error_report <- tibble(idx = run_index[i_run],
                         run = run_name,
                         error = model_output[which(model_output == 'Error:')+1],
                         message = paste(model_output, collapse = '||'))

  output_db <- dbConnect(SQLite(), save_path%//%"error_log"%.%"sqlite")
  dbExecute(output_db, "PRAGMA busy_timeout = 10000")

  dbWriteTable(output_db, run_name, error_report)

  dbDisconnect(output_db)
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
  if(is.null(save_path)) {
    save_path <- project_path
  }
  save_path <- save_path%//%save_dir

  if(!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
  }

  return(save_path)
}

#' Initialize the data base wher model outputs are saved
#'
#' @param save_path Character string. Path of the sql data base
#' @param parameter Parameter set provided for simualtion
#' @param model_setup List with files and variables that define the SWAT model
#'   setup
#'
#' @importFrom DBI dbConnect dbDisconnect dbListTables dbReadTable dbWriteTable
#' @importFrom dplyr mutate select %>%
#' @importFrom lubridate year month day hour minute second
#' @importFrom purrr map_df map_dfc
#' @importFrom RSQLite SQLite
#' @importFrom tibble add_column
#' @keywords internal
#'
initialize_save_file <- function(save_path, parameter, model_setup) {
  output_db <- dbConnect(SQLite(), save_path%//%"par_dat.sqlite")
  table_names <- dbListTables(output_db)

  if("parameter_values"%in% table_names) {
    par_val_db <- dbReadTable(output_db, "parameter_values")
    par_def_db <- dbReadTable(output_db, "parameter_definition")

    if(is.null(parameter)) {
      stop("No parameter set provided to current SWAT run.",
           "A parameter set was however found in 'save_file'.")
    }

    compare_parameter(parameter$values, parameter$definition,
                      par_val_db, par_def_db)

  } else {
    if(!is.null(parameter)){
      if(!is.data.frame(parameter$values)) {
        parameter$values <-  map_dfc(parameter$values, ~.x)
      }

      dbWriteTable(output_db,  "parameter_values", parameter$values)
      dbWriteTable(output_db,  "parameter_definition", parameter$definition)
    }
  }

  date_config <- model_setup[c("start_date", "end_date", "years_skip",
                               "start_date_print", "output_interval")] %>%
    .[!is.na(names(.))] %>%
    map_df(., ~ as.character(.x))

  if(!('start_date_print' %in% names(date_config))) {
    date_config <- add_column(date_config, start_date_print = NA,
                              .after = 'years_skip')
  }

  if("date_config"%in% table_names) {
    date_config_db <- dbReadTable(output_db, 'date_config')
    if(!identical(as.matrix(date_config), as.matrix(date_config_db))) {
      stop("Date settings in the  current SWAT simulations and the",
           "saved date of the simulations in 'save_file' differ!")
    }
  } else {
    dbWriteTable(output_db, 'date_config', date_config)
  }

  dbDisconnect(output_db)
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
#' @importFrom DBI dbConnect dbDisconnect dbListFields
#' @importFrom dplyr filter group_by group_split mutate %>%
#' @importFrom lubridate ymd
#' @importFrom purrr map walk
#' @importFrom RSQLite SQLite
#' @export
#'
load_swat_run <- function(save_dir, variable = NULL, run = NULL,
                          add_parameter = TRUE, add_date = TRUE) {
  save_list <- scan_save_files(save_dir)

  if(add_date) {
    db <- dbConnect(save_list$sim_db[[save_list$sim_tbl$db_id[1]]])
    col_names <- dbListFields(db, save_list$sim_tbl$tbl[1])
    dbDisconnect(db)

    if ('date' %in% col_names) {
      date <- collect_cols('date', save_list$sim_tbl$tbl[1],
                   save_list$sim_db[[save_list$sim_tbl$db_id[1]]]) %>%
        mutate(date = ymd(19700101) + date)
    } else {
      date_config <- save_list$date_config %>%
        mutate(start_date = ymd(start_date),
               end_date   = ymd(end_date))
      date <- get_date_vector_2012(date_config)
    }
  }

  if(is.null(variable)) {
    variable <- save_list$variables
  } else if (any(!(variable %in% save_list$variables))) {
      no_var <- variable[which(!(variable %in% save_list$variables))]
      stop("The following variables wer not simulated and saved in 'save_dir':\n  ",
           paste(no_var, collapse = ', '))
  }

  if(is.null(run)) {
    run <- sort(unique(save_list$sim_tbl$run_idx))
  } else if (any(!(run %in% 1:nrow(save_list$par_val)))) {
    no_run <- run[which(!(run %in% 1:nrow(save_list$par_val)))]
    stop("The following runs are not defined for this simulation project:\n  ",
         paste(no_run, collapse = ', '))
  } else if (any(!(run %in% save_list$sim_tbl$run_idx))) {
    no_run <- run[which(!(run %in% save_list$sim_tbl$run_idx))]
    stop("The following runs are not saved in 'save_dir':\n  ",
         paste(no_run, collapse = ', '))
  }

  parameter <- list(values = save_list$par_val,
                    definition = save_list$par_def)

  sim_tbl_col <- filter(save_list$sim_tbl, run_idx %in% run)

  sim_results <- sim_tbl_col %>%
    group_by(run_name) %>%
    group_split() %>%
    map(., ~ group_by(.x, tbl)) %>%
    map(., ~ group_split(.x)) %>%
    map(., ~ map(.x, ~ collect_cols(variable,.x$tbl, save_list$sim_db[[.x$db_id]]))) %>%
    map(., bind_cols) %>%
    tidy_results(., parameter, date, add_parameter, add_date, run)

  walk(save_list$par_dat_con, ~ dbDisconnect(.x))
  walk(save_list$sim_con, ~ dbDisconnect(.x))

  return(sim_results)
}

#' Collect selected columns from table in SQLite database
#'
#' @param col_name Character vector that defines the column nanems
#' @param tbl_name String that defines the name of the table from which
#'   to collect the columns
#' @param db SQLite database connection
#'
#' @importFrom DBI dbClearResult dbConnect dbDisconnect dbFetch dbSendQuery
#' @importFrom dplyr %>%
#' @importFrom RSQLite SQLite
#' @importFrom tibble as_tibble
#' @keywords internal
#'
collect_cols <- function(col_name, tbl_name, db) {
  db <- dbConnect(db)
  var_str <- paste0("SELECT ", paste(col_name , collapse=","), " FROM ", tbl_name)
  tbl_qry <- dbSendQuery(db, var_str)
  tbl <- tbl_qry %>%
    dbFetch(.) %>%
    as_tibble(.)
  dbClearResult(tbl_qry)
  dbDisconnect(db)
  return(tbl)
}

#' Retrieve information on saved SWAT runs
#'
#' Scan one or multiple save folders that belong to the same SWAT simulation and
#' get information on the simulation period, simulated variables and used
#' parameter sets.
#'
#' @param save_dir Character string or vector
#' @importFrom lubridate years ymd
#' @importFrom purrr map walk walk2
#' @importFrom RSQLite dbDisconnect
#' @importFrom tibble tibble as_tibble
#' @export
#'
scan_swat_run <- function(save_dir) {
  save_list <- scan_save_files(save_dir)

  if (is.na(save_list$date_config$start_date_print)) {
    start_date_print <- ymd(save_list$date_config$start_date) +
                        years(as.numeric(save_list$date_config$years_skip))
  } else {
    start_date_print <- save_list$date_config$start_date_print
  }

  if(nchar(save_list$date_config$output_interval) == 1) {
    save_list$date_config$output_interval <-
      case_when(save_list$date_config$output_interval == 'd' ~ 'daily',
                save_list$date_config$output_interval == 'm' ~ 'monthly',
                save_list$date_config$output_interval == 'y' ~ 'yearly')
  }

  cat("Simulation period:\n", save_list$date_config$start_date, "to",
      save_list$date_config$end_date,"\n")

  cat("\n")
  cat("Printed outputs:\n", as.character(start_date_print), "to",
      save_list$date_config$end_date, "with", save_list$date_config$output_interval,
      "time steps.","\n")

  cat("\n")
  cat("Saved variables:\n")
  cat(truncate(save_list$variables, 20), '\n')

  cat("\n")
  cat("Saved runs:\n")
  cat(display_runs(save_list$sim_tbl$run_idx), '\n')

  if (!is.null(save_list$err_run)) {
    cat("\n")
    cat("Unsaved runs due to errors in model execution :\n")
    err_run_idx <- save_list$err_run %>%
      str_remove(., 'run_') %>%
      as.numeric(.)
    cat(display_runs(err_run_idx))
  }

  cat("\n")
  cat("Parameter set:\n")
  if(!is.null(save_list$par_val[[1]])) {
    cat("Parameter values:\n")
    print(save_list$par_val)
    cat("\nParameter definition:\n")
    print(save_list$par_def)
  } else {
    cat("No data set provided in the save files.")
  }
}

#' Scan the save folders of a SWAT run and return all meta data of this
#' simulation
#'
#' @param save_dir Character string or vector of character strings that provide
#'   the path/s to the save folder/s.
#'
#' @importFrom DBI dbConnect dbListFields dbListTables dbReadTable
#' @importFrom dplyr bind_rows collect filter mutate %>%
#' @importFrom purrr map map2 map2_df
#' @importFrom RSQLite SQLite
#' @importFrom stringr str_detect str_remove
#' @importFrom tibble as_tibble tibble
#' @keywords internal
#'
scan_save_files <- function(save_dir) {
  # Acquire the paths of all '.sqlite' in the provided save folder paths
  sq_file <- save_dir %>%
    map(., ~list.files(path = .x, pattern = '.sqlite$', full.names = TRUE)) %>%
    unlist(.)

  # split the files into parameter/date files and simulation files
  par_dat_file <- sq_file[str_detect(sq_file, 'par_dat.sqlite$')]
  sim_file <- sq_file[str_detect(sq_file, 'thread_[:digit:]+.sqlite$')]
  err_file <- sq_file[str_detect(sq_file, 'error_log.sqlite$')]

  par_dat_db <- map(par_dat_file, ~ dbConnect(SQLite(), .x))
  par_available <- map(par_dat_db, ~ 'parameter_values' %in% dbListTables(.x)) %>%
    unlist(.) %>%
    any(.)

  if(par_available) {
    par_val <- map(par_dat_db, ~dbReadTable(.x, 'parameter_values'))
    if(!is_identical(par_val)) {
      walk(par_dat_db, dbDisconnect)
      stop('The parameter sets in the provided save folders differ!')
    }

    par_def <- map(par_dat_db, ~dbReadTable(.x, 'parameter_definition'))
    if(!is_identical(par_def)) {
      walk(par_dat_db, dbDisconnect)
      stop('The parameter definitions in the provided save folders differ!')
    }

    par_val <- as_tibble(par_val[[1]])
    par_def <- as_tibble(par_def[[1]])

    # If parameter is available, date_config should be as well
    date_available <- map(par_dat_db, ~ 'date_config' %in% dbListTables(.x)) %>%
      unlist(.) %>%
      any(.)

    if(!date_available) {
      walk(par_dat_db, dbDisconnect)
      stop("'date_config' is missing in 'save_file'. A reason can be that ",
           "simulations were saved with a 'SWATplusR' version < 0.6. ",
           "To read these simulations downgrade to an older version of ",
           "'SWATplusR' (e.g. version 0.5)!")
    }

    date_config <- map(par_dat_db, ~dbReadTable(.x, 'date_config'))

    if(!is_identical(date_config)) {
      walk(par_dat_db, dbDisconnect)
      stop('The dates in the provided save folders differ!')
    }
    walk(par_dat_db, dbDisconnect)
    date_config <- as_tibble(date_config[[1]])

    par_dat_db <- par_dat_db[[1]]

  }else {
    walk(par_dat_db, dbDisconnect)
    par_val <- NULL
    par_def  <- NULL
    date_config <- NULL
  }


  if (length(sim_file) > 0) {
    sim_db <- map(sim_file, ~ dbConnect(SQLite(), .x))
    sim_tbl <- map(sim_db, ~tibble(tbl = dbListTables(.x))) %>%
      map2_df(., 1:length(.), ~ mutate(.x, db_id = .y)) %>%
      mutate(run_name = str_remove(tbl, '\\_[:digit:]+$'),
             run_idx  = str_remove(run_name, 'run\\_') %>% as.numeric(.),
             .before  = 1)

    if(nrow(sim_tbl) > 0) {
      first_run <- filter(sim_tbl, run_name == sim_tbl$run_name[1])
      variables <-   map(first_run$tbl, ~ dbListFields(sim_db[[first_run$db_id[1]]], .x)) %>%
        unlist(.) %>%
        .[. != 'date']
    } else {
      variables <- NULL
    }
    walk(sim_db, dbDisconnect)
  } else {
    sim_db <- NULL
    sim_tbl <- NULL
  }

  if (length(err_file) > 0) {
    err_db <- map(err_file, ~ dbConnect(SQLite(), .x))
    err_run <- map(err_db, ~tibble(tbl = dbListTables(.x)))
    walk(err_db, dbDisconnect)

  } else {
    err_db <- NULL
    err_run <- NULL
  }

  return(list(par_val     = par_val,
              par_def     = par_def,
              date_config = date_config,
              variables   = variables,
              sim_tbl     = sim_tbl,
              err_run     = err_run,
              par_dat_db  = par_dat_db,
              sim_db      = sim_db,
              err_db      = err_db))
}

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

#' Convert the information on available runs for the simulated variables into
#' strings that are printed
#'
#' @param tbl overview table that provides meta data for all simulation runs for
#'   all variables saved in the data bases
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map map2 map2_chr
#' @keywords internal
#'
display_runs <- function(runs) {
  diff_runs <- diff(sort(runs))

  end_seq   <- unique(c(runs[diff_runs != 1], runs[length(runs)]))
  start_seq <- unique(c(runs[1], runs[which(diff_runs != 1) + 1]))

  map2_chr(start_seq, end_seq, ~paste_runs(.x, .y)) %>%
    truncate(., 10, side = 'both')
}

#' Paste run indexes if start and end of sequence differ. Otherwise only use
#' start value
#'
#' @param strt Numeric start value of sequence
#' @param end  Numeric end value of sequence
#'
#' @keywords internal
#'
paste_runs <- function(strt, end) {
  if(strt == end) {
    as.character(strt)
  } else {
    paste(strt, end, sep = ':')
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
check_saved_data <- function(save_path, parameter, output, run_index, model_setup) {
  if(length(dir(save_path)) > 0) {
    saved_data <- scan_save_files(save_path)

    compare_parameter(parameter$values, parameter$definition,
                      saved_data$par_val, saved_data$par_def)

    if(nrow(saved_data$sim_tbl) > 0) {
      sim_all   <- 1:nrow(saved_data$par_val)

      sim_compl <- saved_data$sim_tbl$run_idx[saved_data$sim_tbl$run_idx %in% run_index]
      sim_msg <-  sim_compl %>%
        sort() %>%
        display_runs(.)

      sim_miss  <- sim_all[! sim_all %in% sim_compl]
      miss_msg <-  sim_miss %>%
        sort() %>%
        display_runs(.)
      # sim_msg <-  truncate(sim_compl, 10)
        # map(., as.character) %>%

      #   map(., ~ truncate(.x , 10)) %>%
      #   map2(., names(.), ~ paste0("  ",.y, ": ", .x,  " \n")) %>%
      # unlist()

      if(length(unlist(sim_compl)) > 0) {
        if (length(sim_miss) == 0) {
          stop("All simulation runs for the defined parameter set are",
               " already saved in 'save_file'!"
               )
        } else {
          stop("The following simulation runs are already saved in 'save_file': \n",
               '  ', sim_msg,
               "\n  The following simulation runs are missing in 'save_file': \n",
               '  ', miss_msg,
               "\n  Change the 'run_index' to run only missing simulations.")
        }
      }
    }
  }
}


#' Compare if parameters saved in the database and parameters of simulation
#' match
#'
#' @param par_val Parameter values table in simulation
#' @param par_val_db Parameter values table in data base
#' @param par_def Parameter definition table in simulation
#' @param par_def_db Parameter definition table in data base
#'
#' @keywords internal
#'
compare_parameter <- function(par_val, par_def, par_val_db, par_def_db) {
  if(!is.null(par_val_db)) {
    if(!identical(as.matrix(par_val),
                  as.matrix(par_val_db))) {
      stop("Parameters of current SWAT simulations and the parameters"%&&%
             "saved in 'save_file' differ!")
    }
    if(!identical(as.matrix(par_def),
                  as.matrix(par_def_db))) {
      stop("Parameter definition of current SWAT simulation and the"%&&%
             "parameter definition saved in 'save_file' differ!")
    }
  }
}

#' Truncate long character vectors
#'
#' @param x Character vector
#' @param n Threshold value when truncation should be done
#' @param side side of truncation with ... Either left or both
#'
#' @keywords internal
#'
truncate <- function(x, n, side = 'left') {
  if (side == 'left') {
    if(!is.na(x[n])) {
      x <- c(x[1:n],"...")
    }
  } else if (side == 'both') {
    if(length(x) > (n + 1)) {
      x <- c(x[1:(n/2)],"...", x[(length(x) - (n/2)) : length(x)])
    }
  }

  paste(x, collapse = ", ")
}
