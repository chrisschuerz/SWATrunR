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
