library(tidyverse)
library(pasta)
library(SWATplusR)

proj_path <- "D:/SWATplus/LREW_varratio"

setup_swatvari <- function(proj_path, n_prl, prl_path) {
  pro_files <- list.files(proj_path, full.names = T)
  pro_files <- pro_files[!grepl(".txt$",pro_files)]
  swat_exe <- list.files(proj_path) %>%
    .[grepl(".exe$",.)]
  for(i in 1:n_prl) {
    prl_thread <- prl_path%//%"lrew_vari"%_%i%//%".model_run"%//%"thread_1"
    dir.create(prl_thread, recursive = T)
    file.copy(pro_files, prl_thread)

    swat_bat <- c("@echo off",
                  substr(prl_path, 1, 2),
                  paste("cd", prl_thread),
                  swat_exe,
                  "if %errorlevel% == 0 exit 0",
                  "echo.")
    write_lines(swat_bat, prl_thread%//%"swat_run.bat")
  }
}

modify_rtu_con <- function(proj_path, rtu_val, res_val) {
  rtu_con <- read_table(file = proj_path%//%"rout_unit.con", skip = 1,
                        col_types = cols(
                          .default = col_character(),
                          out_tot = col_integer(),
                          frac_1 = col_double(),
                          frac_2 = col_double(),
                          frac_3 = col_double(),
                          frac_4 = col_double(),
                          frac_5 = col_double(),
                          frac_6 = col_double()))

  rtu_con <- rtu_con %>%
    mutate(frac_6 = ifelse(out_tot == 6, frac_6 + res_val, frac_6),
           frac_6 = ifelse(frac_6 > 0.98 & out_tot == 6, 0.98, frac_6),
           frac_6 = ifelse(frac_6 < 0.01 & out_tot == 6, 0.01, frac_6),
           frac_5 = frac_6,
           frac_4 = 1 - frac_6,
           frac_3 = ifelse((out_tot > 2) & ((frac_1 + frac_3) > 0),
                           frac_4*frac_3/(frac_1 + frac_3) + rtu_val, frac_3),
           frac_3 = ifelse((out_tot > 2) & ((frac_3 + frac_5) > 0.99),
                           0.99 - frac_5, frac_3),
           frac_3 = ifelse((out_tot > 2) & (frac_3 < 0.01) & !((frac_1 + frac_3) == 0),
                           0.01, frac_3),
           frac_1 = ifelse(out_tot > 2, 1 - frac_5 - frac_3, frac_1)) %>%
    mutate(frac_1 = frac_1 %>% round(., digits = 2) %>% sprintf("%4.2f", .),
           frac_2 = frac_2 %>% round(., digits = 2) %>% sprintf("%4.2f", .),
           frac_3 = frac_3 %>% round(., digits = 2) %>% sprintf("%4.2f", .),
           frac_4 = frac_4 %>% round(., digits = 2) %>% sprintf("%4.2f", .),
           frac_5 = frac_5 %>% round(., digits = 2) %>% sprintf("%4.2f", .),
           frac_6 = frac_6 %>% round(., digits = 2) %>% sprintf("%4.2f", .))

  nchar_col <- map_int(rtu_con, ~ nchar(.x) %>% max(.)) %>%
    map2_int(., nchar(names(rtu_con)), ~max(.x,.y)) %>%
    map_int(., ~.x + 3L)

  rtu_con <- map2_df(rtu_con, nchar_col, ~ sprintf("%"%&%.y%&%"s", .x))

  write_lines(x = "rout_unit.con: Routing unit connections - LREW variable ratio",
              path = proj_path%//%"rout_unit.con")
  write_lines(x = map2_chr(names(rtu_con),
                           nchar_col,
                           ~ sprintf("%"%&%.y%&%"s", .x)) %>%
                paste(., collapse = " "),
              path = proj_path%//%"rout_unit.con",
              append = TRUE)
  write.table(x = rtu_con, file = proj_path%//%"rout_unit.con",
              append = TRUE,
              quote = FALSE,
              sep = " ",
              row.names = FALSE,
              col.names = FALSE)
}


par_con <- readRDS("D:/Projects_R/SWATplusR_testing/LREW/par_wb.rds")
par_con$abschg_rtu <- runif(1000, -0.5, 0.25)
par_con$abschg_res <- runif(1000, -0.5, 0.5)

par_con_test <- par_con

run_dir <- "D:/SWATplus"
i <- 1
run_thread <- "lrew_vari"%_%i



library(parallel)
library(doSNOW)


run_lrew_vari <- function(pro_path, run_path, par, n_thread,
                          start_date, end_date, years_skip) {
  btc_start <- seq(1, nrow(par), n_thread)

  for(i_btc in btc_start) {
    par_btc <- par[i_btc:(i_btch + n_thread - 1), ]
    foreach(i = 1:n_thread) %dopar% {
      thread_path <- run_path%//%"lrew_vari"%_%i
      thread_run  <- thread_path%//%".model_run/thread_1"
      modify_rtu_con(proj_path = thread_run,
                     rtu_val = par_con_test$abschg_rtu[i],
                     res_val = par_con_test$abschg_res[i])
      run_swatplus(project_path = thread_path,
                   output = define_output("basin_wb", "latq", 1),
                   parameter = par_con[i,1:38],
                   # run_path = run_dir%//%run_thread,
                   keep_folder = TRUE,
                   refresh = FALSE,
                   n_thread = 1,
                   save_file = "save_vari",
                   return = FALSE,
                   years_skip = years_skip,
                   start_date = start_date,
                   end_date   = end_date)
      file.copy(pro_path%//%"rout_unit.con",
                run_dir%//%run_thread%//%".model_run/thread_1",
                overwrite = TRUE)


    }
  }



}
