library(lhs)
library(tidyverse)
library(pasta)

a <- load_swat_run(save_dir = "/media/christoph/EC2C-868B/lrew_wb", variable = "cn", run = 1)
par_wb <- a$parameter$values
a <- randomLHS(10,5)

augmentLHS(a, 10)


par_bound <- list(c(-15,10), c(0.02, 0.16), c(10, 150), c(1, 180), c(10, 150), c(1, 20), c(0, 1), c(0, 1), c(10, 1000), c(0.5, 20),
                  c(0.04, 1), c(0.04, 1), c(0, 5), c(-50, 50), c(-50, 50), c(-50, 50), c(-50, 50), c(0.05, 24), c(0, 10), c(0, 10),
                  c(0, 0.3), c(-25, 25), c(-25, 25), c(0.025, 0.15), c(0, 250), c(0, 1), c(-25, 25), c(-25, 25), c(-25, 25),
                  c(-25, 25), c(-25, 25), c(0, 1), c(0, 1), c(0, 1), c(0, 500), c(0, 5000), c(0.02, 0.2), c(0, 500))

par_wb_norm <- par_wb %>%
  map2_df(., par_bound, ~ (.x - .y[1]) / (.y[2] - .y[1]))

par_augm <- par_wb_norm %>%
  as.matrix(.) %>%
  augmentLHS(., 4000)

par_names <- a$parameter$definition %>%
  mutate(par_name = paste(parameter%.%file_name, "change = "%&%change, sep = " | "))

par_wb_augm <- par_augm %>%
  as_tibble(.) %>%
  map2_df(., par_bound, ~ (.x*(.y[2] - .y[1]) + .y[1])) %>%
  set_names(., par_names$par_name)




