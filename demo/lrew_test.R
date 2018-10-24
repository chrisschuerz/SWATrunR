library(tidyverse)
library(lhs)


par_tbl <- read_table("E:/Projects/Kielstau/kielstau20180820/Scenarios/Default/TxtInOut_2aqu/cal_parms_water.cal", skip = 2, col_names = T)

par_names <- paste(par_tbl$NAME, par_tbl$OBJ_TYP, sep = ".")

chg <- c(1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 3, 3, 3, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3)
chg <- case_when(
  chg == 1 ~ "pctchg",
  chg == 3 ~ "absval"
) %>%
  paste("change", ., sep = " = ")

par_names <- paste(par_names, chg, sep = " | ")

par_bound <- list(c(-15,10), c(0.02, 0.16), c(10, 150), c(1, 180), c(10, 150), c(1, 20), c(0, 1), c(0, 1), c(10, 1000), c(0.5, 20),
                  c(0.04, 1), c(0.04, 1), c(0, 5), c(-50, 50), c(-50, 50), c(-50, 50), c(-50, 50), c(0.05, 24), c(0, 10), c(0, 10),
                  c(0, 0.3), c(-25, 25), c(-25, 25), c(0.025, 0.15), c(0, 250), c(0, 1), c(-25, 25), c(-25, 25), c(-25, 25),
                  c(-25, 25), c(-25, 25), c(0, 1), c(0, 1), c(0, 1), c(0, 500), c(0, 5000), c(0.02, 0.2), c(0, 500))

par_wb <- randomLHS(1000, 38) %>%
  as_tibble(.) %>%
  map2_df(., par_bound, ~ (.x*(.y[2] - .y[1]) + .y[1])) %>%
  set_names(., par_names)

saveRDS(par_wb, "E:/Projects/LREW/LREW_SWAT/par_wb.rds")

library(SWATplusR)
library(dplyr)

par_wb <- readRDS("D:/par_wb.rds")
par_wb <- par_wb[1:4,]
# par_wb_augm <- readRDS("D:/SWATplus_offline/par_wb_augm.R")
#
# par_wb_augm <- par_wb_augm[1001:5000, ]
#


run_swatplus(project_path = "D:/SWATplus/TxtInOut_100cha0lsu",
             output = list(q_b =  define_output(file = "channel",
                                                variable = "flo_out",
                                                unit = 79),
                           q_o =  define_output(file = "channel",
                                                variable = "flo_out",
                                                unit = 40),
                           q_n =  define_output(file = "channel",
                                                variable = "flo_out",
                                                unit = 225),
                           q_f =  define_output(file = "channel",
                                                variable = "flo_out",
                                                unit = 250),
                           q_i =  define_output(file = "channel",
                                                variable = "flo_out",
                                                unit = 211),
                           q_k =  define_output(file = "channel",
                                                variable = "flo_out",
                                                unit = 182),
                           q_j =  define_output(file = "channel",
                                                variable = "flo_out",
                                                unit = 194),
                           q_m =  define_output(file = "channel",
                                                variable = "flo_out",
                                                unit = 1),
                           surq =  define_output(file = "lsunit_wb",
                                                 variable = "surq_gen",
                                                 unit = 1:1248),
                           latq =  define_output(file = "lsunit_wb",
                                                 variable = "latq",
                                                 unit = 1:1248),
                           perc =  define_output(file = "lsunit_wb",
                                                 variable = "perc",
                                                 unit = 1:1248),
                           et =  define_output(file = "lsunit_wb",
                                               variable = "et",
                                               unit = 1:1248),
                           tloss =  define_output(file = "lsunit_wb",
                                                  variable = "tloss",
                                                  unit = 1:1248),
                           cn =  define_output(file = "lsunit_wb",
                                               variable = "cn",
                                               unit = 1:1248),
                           sw =  define_output(file = "lsunit_wb",
                                               variable = "sw",
                                               unit = 1:1248),
                           surq_cha =  define_output(file = "lsunit_wb",
                                                     variable = "surq_cha",
                                                     unit = 1:1248),
                           surq_res =  define_output(file = "lsunit_wb",
                                                     variable = "surq_res",
                                                     unit = 1:1248),
                           surq_ls =  define_output(file = "lsunit_wb",
                                                    variable = "surq_ls",
                                                    unit = 1:1248),
                           latq_cha =  define_output(file = "lsunit_wb",
                                                     variable = "latq_cha",
                                                     unit = 1:1248),
                           latq_ls =  define_output(file = "lsunit_wb",
                                                    variable = "latq_ls",
                                                    unit = 1:1248),
                           flo_aq =  define_output(file = "aquifer",
                                                   variable = "flo",
                                                   unit = 1:1248),
                           rchrg_aq =  define_output(file = "aquifer",
                                                     variable = "rchrg",
                                                     unit = 1:1248),
                           seep_aq =  define_output(file = "aquifer",
                                                    variable = "seep",
                                                    unit = 1:1248),
                           flo_ch_aq =  define_output(file = "aquifer",
                                                      variable = "flo_cha",
                                                      unit = 1:1248),
                           flo_res_aq =  define_output(file = "aquifer",
                                                       variable = "flo_res",
                                                       unit = 1:1248),
                           flo_ls_aq =  define_output(file = "aquifer",
                                                      variable = "flo_ls",
                                                      unit = 1:1248)),
             parameter = par_wb,
             start_date = "1988-01-01",
             end_date = "1992-12-31",
             output_interval = "d",
             years_skip = 2,
             save_path = "D:/LREW_test1",
             save_file = "lrew_ls_1k",
             n_thread = 2,
             return_output = FALSE)

# par_wb_rest <- par_wb_augm[1986:4000,]
