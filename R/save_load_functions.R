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
#' @importFrom RSQLite SQLite sqliteSetBusyHandler
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
  sqliteSetBusyHandler(output_db, 1e6)
  dbExecute(output_db, "BEGIN IMMEDIATE")

  dbWriteTable(output_db, run_name, error_report)
  dbExecute(output_db, "COMMIT")

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
#' @param save_path Path of the sql data base for incrementally saving simulations
#' @param parameter Parameter set provided for simulation
#' @param run_info List with meta data on simulation setup
#' @param run_index IDs of parameter sets for which simulations should be run
#'
#' @importFrom DBI dbConnect dbDisconnect dbListTables dbReadTable dbWriteTable
#' @importFrom dplyr bind_cols mutate select %>%
#' @importFrom lubridate year month day hour minute second
#' @importFrom purrr map_chr map_df
#' @importFrom RSQLite SQLite
#' @importFrom tibble add_column
#' @keywords internal
#'
initialize_save_file <- function(save_path, parameter, run_info, run_index) {
  sim_period <-  map_df(run_info$simulation_period, ~ as.character(.x))
  sim_log <- map_df(run_info$simulation_log, ~ as.character(.x))

  if(length(dir(save_path)) > 0) {
    saved_data <- scan_save_files(save_path)

    compare_tables(saved_data$par_val, parameter$values, 'Parameter values')
    compare_tables(saved_data$par_def, parameter$definition, 'Parameter definitions')
    compare_tables(saved_data$sim_period, sim_period, 'Simulation dates', TRUE)
    compare_tables(saved_data$out_def, run_info$output_definition,
                   'Output definitions', TRUE)

    if(nrow(saved_data$sim_tbl) > 0) {
      sim_all   <- 1:nrow(saved_data$par_val)

      sim_compl <- saved_data$sim_tbl$run_idx[saved_data$sim_tbl$run_idx %in% run_index]
      sim_msg <-  sim_compl %>%
        group_values(.)

      sim_miss  <- sim_all[! sim_all %in% unique(saved_data$sim_tbl$run_idx)]
      miss_msg <-  sim_miss %>%
        group_values(.)

      if(length(unlist(sim_compl)) > 0) {
        if (length(sim_miss) == 0) {
          stop("All simulation runs for the defined parameter set are",
               " already saved in 'save_file'!"
          )
        } else {
          stop("The following simulation runs are already saved in 'save_file': \n",
               '  ', sim_msg,
               "\n  The following simulation runs failed or are missing in 'save_file': \n",
               '  ', miss_msg,
               "\n  Change the 'run_index' to run only missing simulations.")
        }
      }
    }
  }

  inputs_db <- dbConnect(SQLite(), save_path%//%"inputs.sqlite")
  table_names <- dbListTables(inputs_db)

  if(!is.null(parameter) & !"parameter_values"%in% table_names) {
    dbWriteTable(inputs_db,  "parameter_values", parameter$values)
    dbWriteTable(inputs_db,  "parameter_definition", parameter$definition)
  }

  if(!"simulation_period"%in% table_names) {
    dbWriteTable(inputs_db, 'simulation_period', sim_period)
  }

  if(!"output_definition"%in% table_names) {
    dbWriteTable(inputs_db, 'output_definition', run_info$output_definition)
  }

  # if(!"run_log"%in% table_names) {
  #   run_log <- tibble(run_id = 1:nrow(parameter$values),
  #                     finished = NA_character_,
  #                     error = NA_character_,
  #                     time_out = NA_character_)
  #   dbWriteTable(inputs_db, 'run_log', run_log)
  # }
  if ('simulation_log' %in% table_names) {
    sim_log_db <- dbReadTable(inputs_db, 'simulation_log')
    sim_log_db <- sim_log_db %>%
      as_tibble(.) %>%
      mutate(run_started = ymd_hms(run_started, tz = Sys.timezone()),
             run_finished = ymd_hms(run_finished, tz = Sys.timezone()),
             run_time = get_time_interval(run_started, run_finished))
    run_info$simulation_log <- bind_rows(sim_log_db, run_info$simulation_log)
  }
  dbWriteTable(inputs_db, 'simulation_log', sim_log, append = TRUE)

  dbDisconnect(inputs_db)

  return(run_info)
}

#' Update a time stamp in the run_log table
#'
#' @param save_path Path of the sql data base for incrementally saving simulations
#' @param run_index IDs of parameter sets for which simulations should be run
#'
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom RSQLite SQLite sqliteSetBusyHandler
#'
#' @keywords internal
#'
update_sim_log <- function(save_path, run_info) {
  inputs_db <- dbConnect(SQLite(), paste0(save_path, "/inputs.sqlite"))
  sim_log <- map_df(run_info$simulation_log, ~ as.character(.x))
  dbWriteTable(inputs_db, 'simulation_log', sim_log, overwrite = TRUE)
  dbDisconnect(inputs_db)
}

#' Update a time stamp in the run_log table
#'
#' @param save_path Path of the sql data base for incrementally saving simulations
#' @param run_index IDs of parameter sets for which simulations should be run
#'
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbSendQuery
#' @importFrom lubridate now
#' @importFrom RSQLite SQLite sqliteSetBusyHandler
#'
#' @keywords internal
#'
# update_run_log <- function(save_path, run_id, col_name) {
#   log_time <- as.character(now())
#   inputs_db <- dbConnect(SQLite(), paste0(save_path, "/inputs.sqlite"))
#   sqliteSetBusyHandler(inputs_db, 1e4)
#   dbExecute(inputs_db, "BEGIN IMMEDIATE")
#
#   dbSendQuery(inputs_db, paste0("UPDATE run_log SET ",  col_name, " = '",
#                                  log_time, "' WHERE run_id = ", run_id, ";"))
#   dbExecute(inputs_db, "COMMIT")
#
#   dbDisconnect(inputs_db)
# }

#' Compare a table in the current SWAT simulation with the one saved in 'save_file'
#'
#' @param tbl_sim Table defined for the current simulation
#' @param tbl_df  Corresponding table saved in the data bases
#' @param txt Text string to be added in the error message.
#'
#' @keywords internal
#'
compare_tables <- function(tbl_sim, tbl_df, txt, convert = FALSE) {
  if(convert) {
    tbl_sim <- as.matrix(tbl_sim)
    tbl_df  <- as.matrix(tbl_df)
  }
  if(!is_identical_tbl(tbl_sim, tbl_df)) {
    stop(txt, " of current SWAT simulation and the ones ",
         "saved in 'save_file' differ!")
  }
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
#' @param add_run_info Logical. If \code{add_run_info = TRUE} the meta information
#'   for the simulation runs is added to the outputs
#'
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom dplyr arrange bind_cols filter mutate %>%
#' @importFrom lubridate now ymd
#' @importFrom purrr map
#' @importFrom RSQLite SQLite
#' @importFrom stringr str_split
#' @export
#'
load_swat_run <- function(save_dir, variable = NULL, run = NULL,
                          add_parameter = TRUE, add_date = TRUE,
                          add_run_info = TRUE) {
  cat('Scan saved runs...')
  save_list <- scan_save_files(save_dir)
  cat('Done!\n')

  if(is.null(variable)) {
    variable <- unique(save_list$variables$variable)
  } else if (any(!(variable %in% save_list$variables$variable))) {
      no_var <- variable[which(!(variable %in% save_list$variables$variable))]
      stop("The following variables wer not simulated and saved in 'save_dir':\n  ",
           paste(no_var, collapse = ', '))
  }

  run_sim <- sort(unique(save_list$sim_tbl$run_idx))
  run_all <- 1:nrow(save_list$par_val)

  if(is.null(run)) {
    run <- sort(unique(save_list$sim_tbl$run_idx))
  } else if (any(!(run %in% run_all))) {
    no_run <- run[which(!(run %in% run_all))]
    stop("The following runs are not defined for this simulation project:\n  ",
         group_values(no_run))
  } else if (any(!(run %in% run_sim))) {
    no_run <- run[which(!(run %in% save_list$sim_tbl$run_idx))]
    stop("The following runs are not saved in 'save_dir':\n  ",
         group_values(no_run))
  }

  cat('Read variables...\n')

  if(!is.null(save_list$err_log)) {
    run_err <- save_list$err_log$idx
    if (any(run %in% run_err)) {
      err_run <- run[which((run %in% run_err & ! run %in% run_sim))]
      message("The following runs had errors and cannot be read and returned:\n  ",
              group_values(err_run), '\n')
    }
  }

  var_sel <- unique(variable)
  if(add_date) {
    has_date <- 'date' %in% save_list$variables$variable
    if(!has_date) {
      message("Variables were saved without 'date' vectors.",
              " Variables will be read and returned without dates.\n")
    }
  } else {
    var_sel <- var_sel[var_sel != 'date']
  }

  var_tbl <- filter(save_list$variables, variable %in% var_sel)
  tbl_ids <- unique(var_tbl$tbl_id)
  sim_tbl <- filter(save_list$sim_tbl, run_idx %in% run & tbl_ids %in% tbl_ids)

  db_ids <- unique(sim_tbl$db_id)

  sim_results <- list()
  i <- 1
  n <- nrow(sim_tbl)
  t0 <- now()
  for(i_db in db_ids) {
    save_list$sim_db[[i_db]] <- dbConnect(save_list$sim_db[[i_db]])
    sim_tbl_i <- filter(sim_tbl, db_id == i_db) %>%
      arrange(tbl_id)

    for (i_row in 1:nrow(sim_tbl_i)) {
      if (is.null(sim_results[[sim_tbl_i$run_name[i_row]]])) {
        sim_results[[sim_tbl_i$run_name[i_row]]] <- list()
      }
      tbl_i <- sim_tbl_i$tbl[i_row]
      tbl_id_i <- sim_tbl_i$tbl_id[i_row]
      tbl_id_i_sub <- as.numeric(str_split(tbl_id_i, '\\.', simplify = TRUE))
      var_tbl_i <- unique(var_tbl$variable[var_tbl$tbl_id %in% tbl_id_i])
      var_i <- var_sel[var_sel %in% var_tbl_i]
      sim_i <- collect_cols(var_i, tbl_i, save_list$sim_db[[i_db]],
                            handle_conn = F)

      if('date' %in% names(sim_i)) {
        sim_i$date <- ymd(19700101) + sim_i$date
      }

      if (tbl_id_i_sub[2] == 1) {
        sim_results[[sim_tbl_i$run_name[i_row]]][[tbl_id_i_sub[1]]] <- sim_i
      } else {
        sim_results[[sim_tbl_i$run_name[i_row]]][[tbl_id_i_sub[1]]] <-
          bind_cols(sim_results[[sim_tbl_i$run_name[i_row]]][[tbl_id_i_sub[1]]], sim_i)
      }
    display_progress_pct(n = i, nmax = n, t0 = t0)
    i <- i + 1
    }
    dbDisconnect(save_list$sim_db[[i_db]])
  }
  finish_progress(nmax = n, t0 = t0, word = 'Table')
  cat('Return simulation results...')
  output_list <- list()


  if(add_parameter) {
    output_list$parameter <- list(values = save_list$par_val,
                                  definition = save_list$par_def)
  }

  output_list$simulation <- sim_results[order(names(sim_results))] %>%
    tidy_simulations(.)

  if (add_run_info) {
    output_list$run_info <- list(simulation_log = save_list$sim_log,
                                 simulation_period = save_list$sim_period,
                                 output_definition = save_list$out_def)
  }

  cat('Done!\n')
  return(output_list)
}

#' Collect selected columns from table in SQLite database
#'
#' @param col_name Character vector that defines the column names
#' @param tbl_name String that defines the name of the table from which
#'   to collect the columns
#' @param db SQLite database connection
#' @param handle_conn Logical value to decide whether data base should be
#'    connected and disconnected
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
    var_str <- paste0("SELECT ", paste(col_name , collapse=","), " FROM '",
                      tbl_name, "'")
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

  cat("Simulation period definition:\n")
  print(save_list$sim_period, n = Inf)

  cat("\n")
  cat("Output variable definition:\n")
  save_list$out_def$file_full <- NULL
  print(save_list$out_def, n = Inf)

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
      cat(group_values(err_log$idx), '\n\n')
    }
  }

  cat("\n")
  cat("Parameter set:\n")
  if(!is.null(save_list$par_val[[1]])) {
    cat("Parameter values:\n")
    print(save_list$par_val)
    cat("\nParameter definition:\n")
    print(save_list$par_def, n = Inf)
  } else {
    cat("No data set provided in the save files.")
  }

  if(return_full) {
    parameter <- list(values = save_list$par_val,
                      definition = save_list$par_def)
    return_list <- list(variables = save_list$out_def,
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
#' @importFrom lubridate ymd ymd_hms
#' @importFrom purrr list_rbind map map2 map2_df walk
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

  if (length(inp_file) == 0 & length(sim_file) == 0) {
    stop("No data bases with the names 'inputs.sqlite' and 'thread_<int>.sqlite'",
         ' were found in the provided paths.')
  }

  inputs_db <- map(inp_file, ~ dbConnect(SQLite(), .x))
  is_correct_version <- map(inputs_db, ~ 'simulation_log' %in% dbListTables(.x)) %>%
    unlist(.) %>%
    any(.)

  if(length(inp_file) == 0 & !is_correct_version) {
    walk(inputs_db, dbDisconnect)
    stop("\nThe tables saved in 'save_file' are not compatible with this version of SWATrunR.\n",
         "A reason can be that the saved simulations were performed with a SWATrunR version < 1.0.0.\n",
         "To read the saved simulations downgrade to older versions of SWATrunR/SWATplusR (e.g. 0.6).")
  }

  sim_period <- map(inputs_db, ~dbReadTable(.x, 'simulation_period'))

  if(!is_identical_lst(sim_period)) {
    walk(sim_period, dbDisconnect)
    stop('The simulation periods in the provided save folders differ!')
  }

  sim_period <- as_tibble(sim_period[[1]]) %>%
    mutate(start_date = ymd(start_date),
           end_date   = ymd(end_date),
           years_skip = as.integer(years_skip))

  if ('start_date_print' %in% names(sim_period)) {
    sim_period <- mutate(sim_period, start_date_print = ymd(start_date_print))
  }

  par_available <- map(inputs_db, ~ 'parameter_values' %in% dbListTables(.x)) %>%
    unlist(.) %>%
    any(.)

  if(par_available) {
    par_val <- map(inputs_db, ~dbReadTable(.x, 'parameter_values'))
    if(!is_identical_lst(par_val)) {
      walk(inputs_db, dbDisconnect)
      stop('The parameter sets in the provided save folders differ!')
    }

    par_def <- map(inputs_db, ~dbReadTable(.x, 'parameter_definition'))

    if(!is_identical_lst(par_def)) {
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

  if(!is_identical_lst(out_def)) {
    walk(inputs_db, dbDisconnect)
    stop('The output definitions in the provided save folders differ!')
  }

  out_def <- as_tibble(out_def[[1]])

  sim_log <- map(inputs_db, ~dbReadTable(.x, 'simulation_log'))

  sim_log <- list_rbind(sim_log) %>%
    tibble(.) %>%
    mutate(run_started  = ymd_hms(run_started, tz = Sys.timezone()),
           run_finished =  ymd_hms(run_finished, tz = Sys.timezone()))

  walk(inputs_db, dbDisconnect)

  inputs_db <- inputs_db[[1]]

  if (length(sim_file) > 0) {
    sim_tbl <- list()
    sim_db <- list()
    for (i in 1:length(sim_file)) {
      sim_db[[i]] <- dbConnect(SQLite(), sim_file[i])
      sim_tbl[[i]] <- tibble(tbl = dbListTables(sim_db[[i]]))

      if(i == 1) {
        tbls_1 <- sim_tbl[[i]]$tbl
        tbl_id  <- str_extract(tbls_1, '[:digit:]+\\.[:digit:]+$')
        var_tbl <- map(tbls_1, ~ dbListFields(sim_db[[i]], .x)) %>%
          map2(., tbl_id,  ~ tibble(variable = .x, tbl_id = .y)) %>%
          list_rbind(.)
      }
      dbDisconnect(sim_db[[i]])
    }

    sim_tbl <- sim_tbl %>%
      map2_df(., 1:length(.), ~ mutate(.x, db_id = .y)) %>%
      mutate(run_name = str_remove(tbl, '\\_[:digit:]+\\.[:digit:]+$'),
             run_idx  = str_remove(run_name, 'run\\_') %>% as.numeric(.),
             tbl_id   = str_extract(tbl, '[:digit:]+\\.[:digit:]+$'),
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
    err_log$message <- str_split(err_log$message, '\\|\\|')
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
              variables       = var_tbl,
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
is_identical_lst <- function(tbl_list) {
  tbl_list %>%
    map2(.,.[1], ~identical(.x,.y)) %>%
    unlist(.) %>%
    all(.)
}


#' Check if two tables are identical
#'
#' @param x First table
#' @param y Second table
#'
#' @keywords internal
#'
is_identical_tbl <- function(x,y) {
  if(all(dim(x) == dim(y))) {
    identical(x,y)
  } else {
    FALSE
  }
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
group_values <- function(vals, sep = ':') {
  if (is.numeric(vals[1])) {
    vals <- sort(vals)
    diff_vals <- diff(vals)

    end_seq   <- unique(c(vals[diff_vals != 1], vals[length(vals)]))
    start_seq <- unique(c(vals[1], vals[which(diff_vals != 1) + 1]))

    map2_chr(start_seq, end_seq, ~paste_runs(.x, .y, sep = sep)) %>%
      truncate(., 10, side = 'both')
  } else {
    paste(vals, collapse = ', ')
  }
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
