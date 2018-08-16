#' Run SWAT in the generated parallel folder structure
#'
#' @param swat_object Name of the resulting \code{swat_object} that is
#'   returned after running SWAT.
#' @param factor_set Factor set resulting from \code{sample_factor()}
#' @param project_path Path of the SWAT project
#' @param n_thread Number of threads to be used for the parallel model run.
#'   n_thread must be smaller or equal the number of thread folders of the
#'   parallel folder strucuture and the number of cores of the computer.
#' @param run_index Provide a numeric vector (e.g.\code{run_index = c(1:100,
#'   110, 115)}) if only a subset of the \code{factor_set} should be used in
#'   current run. This parameter is for example helpful if the model runs
#'   are split to multiple PCs
#' @param output Define the output variables to extract from the SWAT model
#'   runs. See functions \code{define_output()} and \code{output_variable()}
#'   for instructions to define the output.
#' @param save \code{save = TRUE} aditionally saves the resulting
#'   \code{swat_object} to \code{project_path/"swat_object".rds}.
#' @param save_incr Saves the model runs incrementally. In case of an
#'   interruption of the model runs the already performed runs can be
#'   recovered with \code{recover_run()} if \code{save_incr = TRUE}.
#' @param refresh If set \code{TRUE} the thread folders will be refreshed
#'   before executing SWAT runs. This should always be done except for
#'   testing!
#' @return Returns a nested tibble including parameter sets and simulation
#'   results for the defined output variables.
#'
#' @import dplyr
#' @import tibble
#' @import readr
#' @import foreach
#' @import doSNOW
#' @import pasta
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom magrittr %<>% set_colnames set_names
#' @importFrom purrr transpose flatten
#' @export

run_swat <- function(swat_object, project_path, factor_set, output = NULL,
                     run_index = 1:nrow(factor_set), n_thread = NULL,
                     save = TRUE, save_incr = FALSE, refresh = TRUE) {

  #Also allows input of swat_object name without "".
  swat_object <- match.call() %>% .[["swat_object"]] %>% as.character()

  # General checkup ---------------------------------------------------------
  if(!is.null(get0(swat_object, envir = sys.frame(-1)))){
    stop("swat_object with the name '"%&%swat_object%&%
           "' already exists in work space!")
  }

  if(file.exists(project_path%//%swat_object%.%"rds")){
    stop("File '"%&%swat_object%&%".rds' already exists in project_path!")
  }

  list_threaddirs <- list.dirs(project_path, full.names = FALSE,
                               recursive = FALSE)
  n_threaddirs <- length(list_threaddirs[substr(list_threaddirs, 1,6) ==
                                           "thread"])

  if(is.null(n_thread)){
    n_thread <- max(detectCores(), n_threaddirs)
  } else {
    if(n_thread > n_threaddirs){
      stop("Number of set threads is larger than number of parallel folders!")
    }
    if(n_thread > detectCores()){
      stop("Number of set threads is larger than number of cores on this machine!")
    }
  }

  #Sort and check run_indices
  run_index %<>% sort() %>% unique()

  if(!is.numeric(run_index)) stop("run_index must be a numeric vector!")
  if(max(run_index) > nrow(factor_set)) stop("run_index out of bounds")
  if(any(run_index <= 0)) stop("run_index out of bounds")

  #Check if output is defined
  if(is.null(output)){
    stop("No output variable defined!")
  }

  # Refresh txtIO -----------------------------------------------------------
  if(refresh){
    print("Refresh TxtInOut from Backup folders:")
    pb <- txtProgressBar(min = 0, max = n_thread, initial = 0, style = 3)
    bkp_files <- list.files(project_path%//%"thread_1")

    cl <- makeCluster(n_thread)
    registerDoSNOW(cl)

    pb <- txtProgressBar(max = n_thread, initial = 0, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    foreach (i_refr = 1:n_thread, .options.snow = opts)  %dopar% {
      thread_dir <- project_path%//%"thread"%_%i_refr
      file.copy(thread_dir%//%"Backup"%//%bkp_files, thread_dir,
                overwrite = TRUE)
      if(file.exists(project_path%//%"thread"%_%i_refr%//%"fct_lookup.csv")){
        file.remove(project_path%//%"thread"%_%i_refr%//%"fct_lookup.csv")
      }
      if(any(attr(factor_set, "type") == "par")){
        run_bat <- c("@echo off",
                     substr(thread_dir, 1, 2),
                     "cd"%&&%thread_dir,
                     "start /min /w SWAT_Edit.exe",
                     "swat.exe",
                     "if %errorlevel% == 0 exit 0",
                     "echo.")
      } else {
        run_bat <- c("@echo off",
                     substr(thread_dir, 1, 2),
                     "cd"%&&%thread_dir,
                     "swat.exe",
                     "if %errorlevel% == 0 exit 0",
                     "echo.")
      }
      writeLines(run_bat, thread_dir%//%"swat_run.bat")
      setTxtProgressBar(pb, i_refr)
    }

    close(pb)
    stopCluster(cl)
  }
  cat("\n\n")


  # Subfunctions -------------------------------------------------------------
  ## Read the SWAT output file properties, such as line length or var headers
  get_outstruct <- function(i_var, thread_dir) {
    tmp <- list()
    tmp$nchr <- read_lines(file = thread_dir%//%i_var,
                           skip = 8, n_max = 1) %>%
      nchar()
    tmp$wdth <- switch(i_var,
                       "output.rch" = c(7,3,9,6,rep(12, (tmp$nchr - 25)/12)),
                       "output.sub" = c(7,3,9,5,rep(10, (tmp$nchr - 64)/10), 11, rep(10,3)))
    tmp$head <- read_fwf(file = thread_dir%//%i_var, skip = 8, n_max = 1,
                         fwf_widths(tmp$wdth)) %>%
      gsub("Mg/l|mg/L|mg/kg|kg/ha|kg/h|t/ha|mic/L|\\(mm\\)|kg|cms|tons|mg|mm|km2| ", "", .)
    tmp$head[1] <- "FILE"
    return(tmp)
  }

  ## Read SWAT output files from respective thread folder
  read_output <- function(i_var, out_struct, thread_dir) {
    read_fwf(file =thread_dir%//%i_var,
             fwf_widths(widths = out_struct[[i_var]]$wdth,
                        col_names = out_struct[[i_var]]$head),
             skip = 9, guess_max = 3)
  }

  ## Extract variables from SWAT output file according to "output"
  extract_var <- function(out_var_i, out_file, run_i) {
    out_file <- out_file[[out_var_i[2]]]
    extr_tbl <- eval_expr(out_file, out_var_i[3], run_i)
    tbl_colnames <- colnames(extr_tbl)

    name_idx <- strsplit(tbl_colnames, "_") %>%
      lapply(.,as.numeric) %>%
      lapply(., na.exclude) %>%
      unlist

    if(length(name_idx) == 0 | any(name_idx != run_i)){
      colnames(extr_tbl) <- tbl_colnames%_%sprintf('%06d',run_i)
    }
    return(extr_tbl)
  }

  ## Evaluate expression provided as string in output. Defines mutates on out_tbl
  eval_expr <- function(out_tbl, expr, run_i){
    paste("out_tbl", expr, sep = " %>% ") %>%
      parse(text = .) %>%
      eval(.)
  }

  # Initialize parameters, names, and headers -------------------------------
  factor_set$index <- 1:nrow(factor_set)
  attr(factor_set, "type") <- c(attr(factor_set, "type"), "ind")

  out_names <- unique(output$file)
  run_set <- seq(1, length(run_index), n_thread)

  # Parallel routine --------------------------------------------------------
  t0 <- Sys.time()

  run_time <- system.time({
    cl <- makeCluster(n_thread)
    registerDoSNOW(cl)

    writeLines("Progress log of SWAT run:", con = project_path%//%"progress.log")
    cat(paste(sprintf("%10s", "Run:"),
              sprintf("%20s", "ind Runtime:"),
              sprintf("%20s", "tot Runtime:")),
        file = project_path%//%"progress.log", append = TRUE, sep = "\n")

    print("Performing"%&&%length(run_index)%&&%"simulations, parallel on"%&&%
            min(n_thread, length(run_index))%&&%"cores:")
    pb <- progress_estimated(ceiling(length(run_index)/n_thread))
    pb$print()

    for (i_batch in run_set){
      ind_batch <- run_index[i_batch:(i_batch + n_thread - 1)]
      var_subsamp <- factor_set %>%
        filter(index %in% ind_batch)

      par_subsamp <- var_subsamp[attr(var_subsamp, "type") %in%
                                   c("par", "ind")] %>%
        mutate(thread = 1:nrow(.))
      fct_subsamp <- var_subsamp[attr(var_subsamp, "type") %in%
                                   c("fct", "ind")] %>%
        mutate(thread = 1:nrow(.))

      # Start of foreach() --------------------------------------------------
      sim_i <- foreach(i_run = 1:nrow(var_subsamp),
                       .packages = c("dplyr")) %dopar% {
                         t1 <- Sys.time()
                         thread_dir <- project_path%//%"thread"%_%i_run
                         ind <- par_subsamp$index[i_run]

                         fct_set <- fct_subsamp %>%
                           filter(thread == i_run) %>%
                           select(- c(index, thread))


                         if(ncol(fct_set) > 0){
                           if(file.exists(thread_dir%//%"fct_lookup.csv")){
                             fct_lookup <- read.csv(file = thread_dir%//%"fct_lookup.csv",
                                                    stringsAsFactors = FALSE)

                           } else {
                             fct_lookup <- tibble(factor = colnames(fct_set),
                                                  level  = "!Not defined yet!")

                           }
                           for (factor_i in colnames(fct_set)) {
                             source(project_path%//%"factor"%//%factor_i%//%"batch.R", local = TRUE)
                             fct_lookup$level[fct_lookup$factor == factor_i] <-
                               fct_set[[factor_i]]
                           }

                           write.csv(fct_lookup, thread_dir%//%"fct_lookup.csv",
                                     quote = FALSE, na = "Not found", row.names = FALSE)
                         }

                         par_set <- par_subsamp %>%
                           filter(thread == i_run) %>%
                           select(- c(index, thread))

                         if(ncol(par_set) > 0){
                           par_set %>%
                             t %>%
                             write.table(., file = thread_dir%//%"model.in",
                                         quote = FALSE, row.names = TRUE, col.names = FALSE)

                         }

                         system(thread_dir%//%"swat_run.bat")

                         out_structure <- lapply(out_names, get_outstruct, thread_dir) %>%
                           set_names(out_names)

                         output_file <- lapply(out_names, read_output, out_structure, thread_dir) %>%
                           set_names(out_names)

                         sim_lst <- apply(output, 1, extract_var, output_file, ind) %>%
                           set_names(output$label)

                         t2 <- Sys.time()
                         cat(paste(sprintf("%10s", ind),
                                   sprintf("%20s", tdiff2hms(t2, t1)),
                                   sprintf("%20s", tdiff2hms(t2, t0))),
                             file = project_path%//%"progress.log", append = TRUE, sep = "\n")

                         return(sim_lst)
                       }

      if(!exists("out_tbl", inherits = FALSE)){
        out_tbl <- tibble(idx        = 1:nrow(output),
                          key        = output$label,
                          settings   = NA,
                          date       = NA,
                          simulation = lapply(1:nrow(output), function(x) list()) %>%
                            set_names(output$label))

        # Add dates to out_tbl ---------------------------------------------
        if(i_batch == 1){
          file_cio <- readLines(con = project_path%//%"thread_1"%//%"file.cio",
                                warn = FALSE)
          i_print <- as.numeric(substr(file_cio[59], 1, 16))
          n_yrs <- as.numeric(substr(file_cio[8], 1, 16))
          y_start <- as.numeric(substr(file_cio[9], 1, 16))
          jdn_start <- as.numeric(substr(file_cio[10], 1, 16))
          jdn_end <- as.numeric(substr(file_cio[11], 1, 16))
          ny_skip <- as.numeric(substr(file_cio[60], 1, 16))
          sim_date <- seq(as.Date(paste(sprintf("%03d", jdn_start),
                                        (y_start + ny_skip), sep = "-"),
                                  "%j-%Y"),
                          as.Date(paste(sprintf("%03d", jdn_end),
                                        (y_start + n_yrs - 1), sep = "-"),
                                  "%j-%Y"),
                          by = ifelse(i_print == 0, "month", "day"))

        } else {
          sim_date <- NA
        }

        settings <- select(factor_set, -index)
        attr(settings, "type") <-
          attr(settings, "type")[1:(length(attr(settings, "type")) - 1)]

        out_tbl$settings <- lapply(1:nrow(output), function(x) settings) %>%
          set_names(output$label)

        out_tbl$date <- lapply(1:nrow(output), function(x) sim_date) %>%
          lapply(., function(x) tibble(date = x)) %>%
          set_names(output$label)
        out_tbl$date[!output$date] <- NA
      }

      sim_i %<>% transpose() %>% lapply(., function(x) flatten(x))
      out_tbl$simulation <- Map(c, out_tbl$simulation, sim_i)

      if(save_incr){
        if(!dir.exists(project_path%//%"save_increment")){
          dir.create(project_path%//%"save_increment")
          saveRDS(out_tbl, file = project_path%//%"save_increment"%//%"incr"%_%
                    sprintf("%06d", i_batch)%.%"rds")
        } else {
          saveRDS(sim_i,   file = project_path%//%"save_increment"%//%"incr"%_%
                    sprintf("%06d", i_batch)%.%"rds")
        }
      }

      pb$tick()$print()
    }

    # End for loop --------------------------------------------------------
    stopCluster(cl)
  })

  out_tbl$simulation <- lapply(out_tbl$simulation, function(x) as_tibble(x))

  if(save) saveRDS(out_tbl, file = project_path%//%swat_object%.%"rds")

  assign(swat_object, out_tbl, envir = sys.frame(-1))

  if(save_incr){
    unlink(project_path%//%"save_increment", recursive = TRUE)
  }
  pb$stop()
}
