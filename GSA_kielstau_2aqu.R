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



#-------------------------------------------------------------------------------
# Analysis
library(tidyverse)
library(lubridate)
library(forcats)
ks_aq2 <- readRDS("/home/christoph/Documents/projects/SWATplus/Kielstau/ks_2aq_hyd.rds")
q_obs <- read_csv("/home/christoph/Documents/projects/SWATplus/Kielstau/q_18.csv") %>%
  mutate(date = mdy(date))

q_obs_cal <- q_obs %>%
  filter(date >= ymd("2013-10-01")) %>%
  filter(date <= ymd("2016-12-31"))

q_sim_cal <- ks_aq2$simulation$qout %>%
  filter(date >= ymd("2013-10-01")) %>%
  filter(date <= ymd("2016-12-31")) %>%
  select(-date) %>%
  map_df(., ~ .x*10000/86400)


nse <- function(sim, obs) {
  1 - sum((sim - obs)^2)/sum((obs - mean(obs))^2)
}

nse_cal <- map_dbl(q_sim_cal, ~ nse(.x, q_obs_cal$qobs))

nse_crit <- which(nse_cal > 0.85)

gg_dat <- bind_cols(q_obs_cal, q_sim_cal[, nse_crit]) %>%
  gather(., key = "var", value = "discharge", -date)

ggplot(data = gg_dat) +
  geom_line(aes(x = date, y = discharge, col = var)) +
  theme_bw()

# Validation:
q_obs_val <- q_obs %>%
  filter(date >= ymd("2010-10-01")) %>%
  filter(date <= ymd("2013-09-30"))

q_sim_val <- ks_aq2$simulation$qout %>%
  filter(date >= ymd("2010-10-01")) %>%
  filter(date <= ymd("2013-09-30")) %>%
  select(-date) %>%
  map_df(., ~ .x*10000/86400)

nse_val <- map_dbl(q_sim_val, ~ nse(.x, q_obs_val$qobs))

gg_dat_val <- bind_cols(q_obs_val, q_sim_val[, nse_crit]) %>%
  gather(., key = "var", value = "discharge", -date)

ggplot(data = gg_dat_val) +
  geom_line(aes(x = date, y = discharge, col = var)) +
  theme_bw()

#------------------------------------------------------------------------------
# Sensitivity analysis using PAWN:

binned_pawn <- function(par_tbl, obj, n_bin) {
  par_bin <- par_tbl %>%
    map(., ~cut(.x, breaks = n_bin, labels = FALSE))

  par_name <- names(par_bin)

  map(par_bin, ~tibble(eval = obj, par = .x)) %>%
    map(., ~ group_by(.x, par)) %>%
    map(., ~ summarise(.x, eval = ks.test(eval, obj)$statistic)) %>%
    map_df(., ~ quantile(.x$eval, probs = c(0,.50,1))) %>%
    t() %>%
    as_tibble() %>%
    add_column(factor = par_name, .before = 1) %>%
    set_names(c("par", "T_min", "T_median", "T_max")) %>%
    arrange(., desc(T_median))
}

derive_fdc <- function(q_day){
  if(is.null(dim(q_day))){
    fdc <- data.frame(p = (seq(1,length(q_day))/(length(q_day) + 1)),
                      q = sort(q_day, decreasing = TRUE))
  } else{
    fdc <- data.frame(p = (seq(1,dim(q_day)[1])/(dim(q_day)[1] + 1)),
                      apply(q_day, 2, sort, decreasing = TRUE))
  }
  return(fdc)
}

fdc_q_cal <- q_sim_cal %>%
  map(., ~derive_fdc(.x))

p_thres <- map_int(c(0, 5, 20, 70, 95, 100), ~ which.min(abs(fdc_q_cal[[1]]$p*100 - .x)))

fdc_q_thres <- map(fdc_q_cal, ~.x$q[p_thres]) %>%
  bind_rows() %>%
  t(.) %>%
  as_tibble(.) %>%
  set_names(c("q_max","q_05", "q_20", "q_70", "q_95", "q_min"))

pawn_q_fdc <- map(fdc_q_thres, ~binned_pawn(par_tbl = ks_aq2$parameter$values,
                                          obj = .x, n_bin = 25))

gg_q_fdc <- pawn_q_fdc %>%
  map2(.,names(.),   ~ mutate(.x, fdc_prob = .y)) %>%
  bind_rows(.) %>%
  select(par, T_median, fdc_prob) %>%
  mutate(par = fct_reorder(par, T_median, max),
         fdc_prob = factor(fdc_prob, levels = c("q_max","q_05", "q_20", "q_70", "q_95", "q_min")))

ggplot(data = gg_q_fdc) +
  geom_point(aes(x = par, y = T_median, col = fdc_prob)) +
  coord_flip() +
  scale_color_brewer(palette = "Spectral") +
  xlab("Parameter") +
  ylab("Sensitivity surface flow") +
  theme_bw()

q_til_cal <- ks_aq2$simulation$qtil %>%
  filter(date >= ymd("2013-10-01")) %>%
  filter(date <= ymd("2016-12-31")) %>%
  select(-date) %>%
  map_df(., ~ .x*10000/86400)

fdc_qtil_cal <- q_til_cal %>%
  map(., ~derive_fdc(.x))

fdc_qtil_thres <- map(fdc_qtil_cal, ~.x$q[p_thres]) %>%
  bind_rows() %>%
  t(.) %>%
  as_tibble(.) %>%
  set_names(c("q_max","q_05", "q_20", "q_70", "q_95", "q_min"))

pawn_qtil_fdc <- map(fdc_qtil_thres, ~binned_pawn(par_tbl = ks_aq2$parameter$values,
                                            obj = .x, n_bin = 25))


gg_qtil_fdc <- pawn_qtil_fdc %>%
  map2(.,names(.),   ~ mutate(.x, fdc_prob = .y)) %>%
  bind_rows(.) %>%
  select(par, T_median, fdc_prob) %>%
  mutate(par = fct_reorder(par, T_median, max),
         fdc_prob = factor(fdc_prob, levels = c("q_max","q_05", "q_20", "q_70", "q_95", "q_min")))

ggplot(data = gg_qtil_fdc) +
  geom_point(aes(x = par, y = T_median, col = fdc_prob)) +
  coord_flip() +
  scale_color_brewer(palette = "Spectral") +
  xlab("Parameter") +
  ylab("Sensitivity tile flow") +
  theme_bw()


eta_cal <- ks_aq2$simulation$eta %>%
  filter(date >= ymd("2013-10-01")) %>%
  filter(date <= ymd("2016-12-31")) %>%
  select(-date) %>%
  map_df(., ~ .x*10000/86400)

fdc_eta_cal <- eta_cal %>%
  map(., ~derive_fdc(.x))

fdc_eta_thres <- map(fdc_eta_cal, ~.x$q[p_thres]) %>%
  bind_rows() %>%
  t(.) %>%
  as_tibble(.) %>%
  set_names(c("q_max","q_05", "q_20", "q_70", "q_95", "q_min"))

pawn_eta_fdc <- map(fdc_eta_thres, ~binned_pawn(par_tbl = ks_aq2$parameter$values,
                                                  obj = .x, n_bin = 25))


gg_eta_fdc <- pawn_eta_fdc %>%
  map2(.,names(.),   ~ mutate(.x, fdc_prob = .y)) %>%
  bind_rows(.) %>%
  select(par, T_median, fdc_prob) %>%
  mutate(par = fct_reorder(par, T_median, max),
         fdc_prob = factor(fdc_prob, levels = c("q_max","q_05", "q_20", "q_70", "q_95", "q_min")))

ggplot(data = gg_eta_fdc) +
  geom_point(aes(x = par, y = T_median, col = fdc_prob)) +
  coord_flip() +
  scale_color_brewer(palette = "Spectral") +
  xlab("Parameter") +
  ylab("Sensitivity ETA") +
  theme_bw()
