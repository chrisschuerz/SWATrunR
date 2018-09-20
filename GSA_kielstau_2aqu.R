pro_path <- "C:/Kielstau_2aqu/TxtInOut_2aqu"

n_run <- 1000

parameter <- tibble::tibble(
  "cn2.hru|change = pctchg" =  runif(n_run, -25, 10),
  "ovn.hru|change = absval" =  runif(n_run, 0.01, 20),
  "slope_len.hru|change = absval" = runif(n_run, 10, 100),
  "lat_ttime.hru|change = absval" = runif(n_run, 1, 100),
  "canmx.hru|change = absval" = runif(n_run, 1, 30),
  "esco.bsn|change = absval" = runif(n_run, 0.1, 0.9),
  "epco.bsn|change = absval" = runif(n_run, 0.1, 0.9),
  "dep_imp.hru|change = absval" = runif(n_run, 0, 6000),
  "bd.sol|change = pctchg" = runif(n_run, -50, 30),
  "awc.sol|change = pctchg" = runif(n_run, -50, 50),
  "k.sol|change = pctchg" = runif(n_run, -50, 50),
  "surlag.bsn|change = absval" = runif(n_run, 0.1, 24),
  "k_ch.rte|change = absval" = runif(n_run, 0, 300),
  "alpha_bnk.rte|change = absval" = runif(n_run, 0, 0.9),
  "alpha.gw|change = absval" = runif(n_run, 0, 0.9),
  "delay.gw|change = absval" = runif(n_run, 0, 350),
  "flow_min.gw|change = absval" = runif(n_run, 0, 5000),
  "revap_co.gw|change = absval" = runif(n_run, 0.02, 0.2),
  "revap_min.gw|change = absval" = runif(n_run, 0, 500))

parameter <- tibble::tibble(
  "cn2.hru|change = pctchg" =  runif(n_run, -25, 10),
  "ovn.hru|change = absval" =  runif(n_run, 0.01, 20),
  "slope_len.hru|change = absval" = runif(n_run, 10, 100),
  "lat_ttime.hru|change = absval" = runif(n_run, 1, 100),
  "canmx.hru|change = absval" = runif(n_run, 1, 30),
  "esco.bsn|change = absval" = runif(n_run, 0.1, 0.9),
  "epco.bsn|change = absval" = runif(n_run, 0.1, 0.9),
  "awc.sol|change = pctchg" = runif(n_run, -50, 50),
  "k.sol|change = pctchg" = runif(n_run, -50, 50),
  "surlag.bsn|change = absval" = runif(n_run, 0.1, 24),
  "k_ch.rte|change = absval" = runif(n_run, 0, 300),
  "alpha_bnk.rte|change = absval" = runif(n_run, 0, 0.9))

ks_aq2_hyd_test <- run_swatplus(
  project_path = pro_path,
  parameter = parameter,
  output = list(qout = define_output(file = "channel",
                                     variable = "floout",
                                     unit = 18),
                latq = define_output(file = "basin_wb",
                                     variable = "latq",
                                     unit = 1),
                waty = define_output(file = "basin_wb",
                                     variable = "wtryld",
                                     unit = 1),
                perc = define_output(file = "basin_wb",
                                     variable = "perc",
                                     unit = 1),
                tlos = define_output(file = "basin_wb",
                                     variable = "tloss",
                                     unit = 1),
                eta  = define_output(file = "basin_wb",
                                     variable = "et",
                                     unit = 1),
                sw   = define_output(file = "basin_wb",
                                     variable = "sw",
                                     unit = 1),
                qtil = define_output(file = "basin_wb",
                                     variable = "qtile",
                                     unit = 1)),
  start_date = "2007-01-01", end_date = "2016-12-31", years_skip = 3, n_thread = 4)

library(tidyverse)

test$simulation$floout %>%
  gather(key = "run", value = "discharge", - date) %>%
  ggplot(data = .) +
  geom_line(aes(x = date, y = discharge, col = run))
