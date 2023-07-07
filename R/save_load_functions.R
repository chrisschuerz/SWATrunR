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
#' @importFrom purrr map map2 set_names walk2
#' @importFrom RSQLite SQLite
#' @keywords internal
#'
save_run <- function(save_path, model_output, parameter, run_index, i_run, i_thread) {
  n_digit <- get_digit(parameter$values)
  run_name <- "run"%_%sprintf("%0"%&%n_digit%&%"d", run_index[i_run])

  model_output <- set_names(model_output, run_name%_%1:length(model_output))
  # Convert date to numeric if date column is in table
  has_date <- which(map_lgl(model_output, ~ 'date' %in% names(.x)))

  if('date' %in% colnames(model_output)) {
    model_output[has_date] <- map(model_output[has_date],
                                  ~ mutate(.x, date = as.integer(date)))
  }

  # Splitting output table to 2000 column pieces due to column number limit
  # of SQLite
  out_split <- map(model_output, ~ split_out_tbl(.x, 2000)) %>%
    list_flatten(., name_spec = "{outer}{inner}")

  output_db <- dbConnect(SQLite(), save_path%//%i_thread%.%"sqlite")

  walk2(out_split, names(out_split), ~dbWriteTable(output_db, .y, .x))

  dbDisconnect(output_db)
}

#' Split tables with more than ncol_max columns into list of tables
#'
#' @param tbl Table which should be split
#' @param ncol_max Maximum number of columns before split is done
#'
#' @importFrom purrr map2 set_names
#' @keywords internal
#'
split_out_tbl <- function(tbl, ncol_max) {
  col_split <- c(0:floor(ncol(tbl)/ncol_max)*ncol_max, ncol(tbl))
  out_split <- map2(col_split[-length(col_split)] + 1, col_split[-1],
                    ~ tbl[,.x:.y]) %>%
    set_names(paste0('.',1:(length(col_split) - 1)))
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
#' @param run_info List with meta data on simulation
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
initialize_save_file <- function(save_path, parameter, run_info) {
  output_db <- dbConnect(SQLite(), save_path%//%"inputs.sqlite")
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

  sim_period <- run_info$simulation_period %>%
    map_df(., ~ as.character(.x))

  if("simulation_period"%in% table_names) {
    sim_period_db <- dbReadTable(output_db, 'simulation_period')
    if(!identical(as.matrix(sim_period), as.matrix(sim_period_db))) {
      stop("Date settings in thecurrent SWAT simulations and the",
           "saved date of the simulations in 'save_file' differ!")
    }
  } else {
    dbWriteTable(output_db, 'simulation_period', sim_period)
  }

  out_def <- run_info$output_definition

  if("output_definition"%in% table_names) {
    out_def_db <- dbReadTable(output_db, 'output_definition')
    if(!identical(as.matrix(out_def), as.matrix(out_def_db))) {
      stop("The defined outputs in the current SWAT simulations and the",
           "saved output definition in 'save_file' differ!")
    }
  } else {
    dbWriteTable(output_db, 'output_definition', out_def)
  }

  sim_log <- run_info$simulation_log
  if("simulation_log"%in% table_names) {
    sim_log_db <- dbReadTable(output_db, 'simulation_log')
    sim_log <- bind_rows(sim_log_db, sim_log)
  }
  dbWriteTable(output_db, 'simulation_log', sim_log)

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
#' @importFrom purrr map map_lgl walk
#' @importFrom RSQLite SQLite
#' @importFrom stringr str_remove
#' @export
#'
load_swat_run <- function(save_dir, variable = NULL, run = NULL,
                          add_parameter = TRUE, add_date = TRUE) {
  cat('Scan saved runs...')
  save_list <- scan_save_files(save_dir)
  cat('Done!\n')

  if(add_date) {
    cat('Read date vector...')
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
   cat('Done!\n')
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

  if(add_parameter) {
    cat('Read parameters...')
    cat('Done!\n')
  }
  parameter <- list(values = save_list$par_val,
                    definition = save_list$par_def)

  cat('Read variables...\n')
  variable <- map(save_list$variables_split, ~ variable[variable %in% .x])
  tbl_ids <- which(map_lgl(variable, ~ length(.x) > 0))

  sim_tbl_col <- filter(save_list$sim_tbl, run_idx %in% run) %>%
    mutate(., tbl_id = as.numeric(str_remove(tbl, 'run_[:digit:]+_')))

  if(!is.na(sim_tbl_col$tbl_id[1])) {
    sim_tbl_col <- filter(sim_tbl_col, tbl_id %in% tbl_ids)
  }

  # sim_tbl_list <- sim_tbl_col %>%
  #   group_by(run_name) %>%
  #   group_split() %>%
  #   map(., ~ group_by(.x, tbl)) %>%
  #   map(., ~ group_split(.x))
  #
  db_ids <- unique(sim_tbl_col$db_id)

  sim_results <- list()
  i <- 1
  n <- nrow(sim_tbl_col)
  t0 <- now()
  for(i_db in db_ids) {
    save_list$sim_db[[i_db]] <- dbConnect(save_list$sim_db[[i_db]])
    sim_tbl_col_i <- filter(sim_tbl_col, db_id == i_db)
    for (i_row in 1:nrow(sim_tbl_col_i)) {
      if (is.null(sim_results[[sim_tbl_col_i$run_name[i_row]]])) {
        sim_results[[sim_tbl_col_i$run_name[i_row]]] <- list()
      }
      sim_results[[sim_tbl_col_i$run_name[i_row]]][[sim_tbl_col_i$tbl[i_row]]] <-
        collect_cols(variable[[sim_tbl_col_i$tbl_id[i_row]]],
                     sim_tbl_col_i$tbl[i_row],
                     save_list$sim_db[[i_db]],
                     handle_conn = F)
    display_progress_pct(n = i, nmax = n, t0 = t0)
    i <- i + 1
    }
    dbDisconnect(save_list$sim_db[[i_db]])
  }
  finish_progress(nmax = n, t0 = t0, word = 'Table')
  cat('Return simulation results...')
  sim_results <-  map(sim_results, bind_cols)
  sim_results <- sim_results[order(names(sim_results))] %>%
    tidy_results(., parameter, date, add_parameter, add_date, run)
  cat('Done!\n')
  return(sim_results)
}

#' Collect selected columns from table in SQLite database
#'
#' @param col_name Character vector that defines the column names
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
collect_cols <- function(col_name, tbl_name, db, handle_conn = TRUE) {
  if (length(col_name) > 0) {
    if (handle_conn) {
      db <- dbConnect(db)
    }
    var_str <- paste0("SELECT ", paste(col_name , collapse=","), " FROM ", tbl_name)
    tbl_qry <- dbSendQuery(db, var_str)
    tbl <- tbl_qry %>%
      dbFetch(.) %>%
      as_tibble(.)
    dbClearResult(tbl_qry)
    if (handle_conn) {
      dbDisconnect(db)
    }
  } else {
    tbl <- NULL
  }
  return(tbl)
}

#' Retrieve information on saved SWAT runs
#'
#' Scan one or multiple save folders that belong to the same SWAT simulation and
#' get information on the simulation period, simulated variables and used
#' parameter sets.
#'
#' @param save_dir Character string or vector
#' @param return_full If TRUE a list with all variables, successful run IDs and
#'   missing run IDs is returned. Otherwise only the overview is printed.
#'
#' @importFrom lubridate years ymd
#' @importFrom purrr map walk walk2
#' @importFrom RSQLite dbDisconnect
#' @importFrom tibble tibble as_tibble
#' @export
#'
scan_swat_run <- function(save_dir, return_full = FALSE) {
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
  cat(truncate(group_variable_units(save_list$variables), 20), '\n')

  run_index <- sort(unique(save_list$sim_tbl$run_idx))
  run_index_miss <- which(! 1:nrow(save_list$par_val) %in% run_index)

  if (length(run_index) > 0) {
    cat("\n")
    cat("Saved runs:\n")
    cat(group_values(run_index), '\n')
  } else {
    cat("\nNo successful simulations saved so far.\n")
  }

  if (length(run_index_miss) > 0) {
    cat("\n")
    cat("Missing runs:\n")
    cat(group_values(run_index_miss), '\n')
  } else {
    cat("\nNo missing runs. All simulations successful.\n")
  }

  if (!is.null(save_list$err_log)) {
    err_log <- save_list$err_log %>%
      filter(! idx %in% run_index)
    if (nrow(err_log) > 0) {
      cat("\n")
      cat("Unsaved runs due to errors in model execution :\n")
      cat(group_values(err_run_idx), '\n\n')
    }
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

  if(return_full) {
    parameter <- list(values = save_list$par_val,
                      definition = save_list$par_def)
    return_list <- list(variables = save_list$variables,
                        run_index_finished = run_index,
                        run_index_missing  = run_index_miss,
                        parameter = parameter)
    if(nrow(err_log) > 0) {
      return_list$error_log <- err_log
    }
    return(return_list)
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
#' @importFrom purrr list_rbind map map2 map2_df
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
  inp_file <- sq_file[str_detect(sq_file, 'inputs.sqlite$')]
  sim_file <- sq_file[str_detect(sq_file, 'thread_[:digit:]+.sqlite$')]
  err_file <- sq_file[str_detect(sq_file, 'error_log.sqlite$')]

  inputs_db <- map(inp_file, ~ dbConnect(SQLite(), .x))
  is_correct_version <- map(inputs_db, ~ 'simulation_log' %in% dbListTables(.x)) %>%
    unlist(.) %>%
    any(.)

  if(length(inp_file) == 0 | !is_correct_version) {
    walk(input_db, dbDisconnect)
    stop("\nThe tables saved in 'save_file' are not compatible with this version of SWATrunR.\n",
         "A reason can be that the saved simulations were performed with a SWATrunR version < 1.0.0.\n",
         "To read the saved simulations downgrade to older versions of SWATrunR/SWATplusR (e.g. 0.6).")
  }

  sim_period <- map(inputs_db, ~dbReadTable(.x, 'simulation_period'))

  if(!is_identical(sim_period)) {
    walk(sim_period, dbDisconnect)
    stop('The simulation periods in the provided save folders differ!')
  }

  sim_period <- as_tibble(sim_period[[1]]) %>%
    mutate(start_date = ymd(start_date),
           end_date   = ymd(end_date),
           years_skip = as.numeric(years_skip))

  if ('start_date_print' %in% names(sim_period)) {
    sim_period <- mutate(sim_period, start_date_print = ymd(start_date_print))
  }

  par_available <- map(inputs_db, ~ 'parameter_values' %in% dbListTables(.x)) %>%
    unlist(.) %>%
    any(.)

  if(par_available) {
    par_val <- map(inputs_db, ~dbReadTable(.x, 'parameter_values'))
    if(!is_identical(par_val)) {
      walk(inputs_db, dbDisconnect)
      stop('The parameter sets in the provided save folders differ!')
    }

    par_def <- map(inputs_db, ~dbReadTable(.x, 'parameter_definition'))

    if(!is_identical(par_def)) {
      walk(inputs_db, dbDisconnect)
      stop('The parameter definitions in the provided save folders differ!')
    }

    par_val <- as_tibble(par_val[[1]])
    par_def <- as_tibble(par_def[[1]])

  } else {
    par_val <- NULL
    par_def  <- NULL
  }

  out_def <- map(inputs_db, ~dbReadTable(.x, 'output_definition'))

  if(!is_identical(out_def)) {
    walk(inputs_db, dbDisconnect)
    stop('The output definitions in the provided save folders differ!')
  }

  out_def <- as_tibble(out_def[[1]])

  sim_log <- map(inputs_db, ~dbReadTable(.x, 'simulation_log'))

  sim_log <- list_rbind(sim_log) %>%
    tibble(.) %>%
    mutate(run_started  = ymd_hms("19700101 00:00:00") + run_started,
           run_finished =  ymd_hms("19700101 00:00:00") + run_finished)

  walk(inputs_db, dbDisconnect)

  inputs_db <- inputs_db[[1]]

  if (length(sim_file) > 0) {
    sim_tbl <- list()
    sim_db <- list()
    for (i in 1:length(sim_file)) {
      sim_db[[i]] <- dbConnect(SQLite(), sim_file[i])
      sim_tbl[[i]] <- tibble(tbl = dbListTables(sim_db[[i]]))
      dbDisconnect(sim_db[[i]])
    }

    sim_tbl <- sim_tbl %>%
      map2_df(., 1:length(.), ~ mutate(.x, db_id = .y)) %>%
      mutate(run_name = str_remove(tbl, '\\_[:digit:]+\\.[:digit:]+$'),
             run_idx  = str_remove(run_name, 'run\\_') %>% as.numeric(.),
             .before  = 1)
  } else {
    sim_db <- NULL
    sim_tbl <- NULL
  }

  if (length(err_file) > 0) {
    err_db <- map(err_file, ~ dbConnect(SQLite(), .x))
    err_log <- list()
    for(db_i in err_db) {
      err_tbls <- dbListTables(db_i)
      for (tbl_i in err_tbls) {
        err_log[[tbl_i]] <- dbReadTable(db_i, tbl_i) %>% as_tibble(.)
      }
    }
    err_log <- list_rbind(err_log)
    walk(err_db, dbDisconnect)

  } else {
    err_db <- NULL
    err_log <- NULL
  }

  return(list(par_val         = par_val,
              par_def         = par_def,
              sim_period      = sim_period,
              sim_log         = sim_log,
              out_def         = out_def,
              # variables       = variables,
              # variables_split = variables_split,
              sim_tbl         = sim_tbl,
              err_log         = err_log,
              inputs_db       = inputs_db,
              sim_db          = sim_db,
              err_db          = err_db))
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
group_values <- function(runs, sep = ':') {
  runs <- sort(runs)
  diff_runs <- diff(runs)

  end_seq   <- unique(c(runs[diff_runs != 1], runs[length(runs)]))
  start_seq <- unique(c(runs[1], runs[which(diff_runs != 1) + 1]))

  map2_chr(start_seq, end_seq, ~paste_runs(.x, .y, sep = sep)) %>%
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
paste_runs <- function(strt, end, sep) {
  if(strt == end) {
    as.character(strt)
  } else {
    paste(strt, end, sep = sep)
  }
}

#' Group variable names which where saved for several spatial units
#'
#' @param variable_names Vector with variable names
#'
#' @importFrom dplyr group_by group_split %>%
#' @importFrom purrr list_c map map_chr map2 set_names
#' @importFrom stringr str_extract str_remove
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
group_variable_units <- function(variable_names) {
  var_list <- tibble(variables = variable_names,
                     parent = str_remove(variables, '_[:digit:]+$'),
                     unit = as.numeric(str_extract(variables, '[:digit:]+$'))) %>%
    group_by(parent) %>%
    group_split()

  var_names <- map_chr(var_list, ~ .x$parent[1])
  var_list %>%
    set_names(., var_names) %>%
    map(., ~group_values(sort(.x$unit), sep = ' to ')) %>%
    map2(., names(.), ~ paste(.y, .x, sep = '_')) %>%
    list_c(.) %>%
    str_remove(., '_$')
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
