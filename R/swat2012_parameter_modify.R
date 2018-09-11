
thread_parameter <- swat_parameter

parameter$parameter_constrain



update_par <- function(par, par_up, change){
  case_when(
    change == "rep" ~ par_up,
    change == "rel" ~ par * (1 + par_up),
    change == "abs" ~ par_old + par_up
    )
}



par_new <- backup

par_up <- data.frame(GWQMN_chol = 3,
                     GW_REVAP_chol = 4,
                     REVAPMN_chol = 5,
                     RCHRG_DP_chol = 6,
                     GW_SPYLD_chol = 7,
                     GW_DELAY_chol = 1,
                     ALPHA_BF_chol = 2)


alter_par <- function(par_new, par_up, filter, hru_lst) {
  par_new$file_write <- list()

  for (i in names(filter)){
    filter_i <- filter[[i]]

    update_in <- par_up %>%
      gather(Label, Value) %>%
      right_join(filter_i$Parameter, by = "Label") %>%
      right_join(par_new[[filter_i$File]]$struct, by = "Parameter") %>%
      select(Parameter, Value, Method)

    rows_select <- hru_lst$SUB  %in% filter_i$Subbasin &
      hru_lst$LUSE %in% filter_i$Landuse &
      hru_lst$SOIL %in% filter_i$Soil &
      hru_lst$SLP  %in% filter_i$Slope

    if(sum(rows_select) == 1){
      par_new[[filter_i$File]]$file$par[rows_select,] <-
        mapply(update_par, par_new[[filter_i$File]]$file$par[1,],
               update_in$Value,
               update_in$Method)
    } else if(sum(rows_select) > 1){
      par_new[[filter_i$File]]$file$par[rows_select,] <-as.data.frame(
        mapply(update_par, par_new[[filter_i$File]]$file$par[rows_select,],
               update_in$Value,
               update_in$Method))
    }

    par_new$file_write[[filter_i$File]] <- unique(c(par_new$file_write[[filter_i$File]],
                                                    which(rows_select)))
  }
  return(par_new)
}

