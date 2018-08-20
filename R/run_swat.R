#' Run a SWAT project and return ouputs in R
#'
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

run_swat2012 <- function(project_path, output, parameter = NULL,
                         start_date = NULL, end_date = NULL,
                         output_interval = NULL, years_skip = NULL,
                         run_index = NULL, n_thread = NULL,
                         save = FALSE, save_incr = FALSE, save_file = NULL,
                         return_out = TRUE, refresh = FALSE,
                         keep_folder = FALSE, quiet = FALSE) {

  ## Check all inputs, modify file cio etc, BEFORE very long task of copying!!!


#-------------------------------------------------------------------------------
  # Build folder structure where the model will be executed
  ## Identify the required number of parallel threads to build.
  n_thread <- min(max(nrow(parameter),1),
                  max(n_thread,1),
                  parallel::detectCores())

  ## Case .model_run exists already and no forced refresh considered
  if(dir.exists(project_path%//%".model_run") & !refresh) {
    ## Check how many parallel threads are available
    n_thread_avail <- list.dirs(project_path%//%".model_run") %>%
      substr(.,(nchar(.) - 7), nchar(.)) %>%
      grepl("thread_",.) %>%
      sum()
    ## The existing folder strucuture is used when more parallel folders are
    ## available than parallel threads are needed
    if(n_thread_avail >= n_thread) {
      if(!quiet) {
        message("Model will be executed in existing '.model_run' folder structure"%&%
                "\nMake shure '.model_run' is up to date with the project folder!")
      }
    ## If the number of available parallel folders is not sufficient
    ## a new setup of the folder structures is forced
    } else {
      unlink(project_path%//%".model_run", recursive = TRUE)
      if(!quiet) {
        message("The number of existing threads is lower than the required number."%&%
                "\nParallel folder structure will be created from scratch!"%&&%
                "\nBuilding '.model_run' with"%&&%n_thread%&&%"parallel threads:")
      }
      build_model_run(project_path, n_thread)
    ## Build the parallel folder structure if it does not exist or if a
    ## forced refresh was set with refresh = TRUE
    } else {
      if(!quiet) {
        cat("Building '.model_run' with"%&&%n_thread%&&%"parallel threads:")
      }
      build_model_run(project_path, n_thread)
    }
  }

#-------------------------------------------------------------------------------
#

}
