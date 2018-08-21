#' Write the Model.in file required for rewriting paramters with SWAT_Edit.exe'
#'
#' @param parameter Named parameter vector or parameter table
#' @param thread_path Path to respective thread where parameters are rewritten
#' @param i_run Index of current run to select the current parameter set
#'
#' @keywords internal
#'

write_model_in <- function(parameter, thread_path, i_run){
  if(!is.null(dim(parameter))){
    parameter <- parameter[i_run,]
  }
  data.frame(parameter = names(parameter),
             value = as.numeric(parameter),
             stringsAsFactors = FALSE) %>%
    write.table(., file = thread_path%//%"model.in",
                quote = FALSE, row.names = FALSE, col.names = FALSE)
}
