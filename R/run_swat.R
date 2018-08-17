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
                         output_int = NULL, years_skip = NULL,
                         run_index = NULL, n_parallel = NULL,
                         save = FALSE, save_incr = FALSE, save_file = NULL,
                         return_out = TRUE, keep_folder = FALSE) {
  #Build folder structure where the model will be executed
  ##Check if .model_execution already exists. If yes write message as "warning"
  ##If not build folder structure. n_threads based on n_par, n_cores, n_parallel

  #...

  #
}
