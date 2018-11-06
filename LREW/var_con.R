library(tidyverse)
library(pasta)

lrew_path <- "D:/SWATplus/LREW_varratio"

rtu_con <- read_table(file = lrew_path%//%"rout_unit.con", skip = 1)

f_rtu <- 0.1
f_res <- 0.05

rtu_con <- rtu_con %>%
  mutate(frac_6 = ifelse(out_tot == 6, frac_6 + f_res, frac_6),
         frac_6 = ifelse(frac_6 >= 0.99 & out_tot == 6, 0.99, frac_6),
         frac_6 = ifelse(frac_6 <= 0.01 & out_tot == 6, 0.01, frac_6),
         frac_5 = frac_6,
         frac_4 = 1 - frac_6,
         frac_13 = ifelse(out_tot == 6, 1 - frac_5, 1),
         frac_3 = ifelse(out_tot >= 4, frac_3*frac_13 + f_rtu, frac_3),
         frac_3 = ifelse(frac_3 >= 0.99 & out_tot >= 4, 0.99, frac_3),
         frac_3 = ifelse(frac_3 <= 0.01 & out_tot >= 4, 0.01, frac_3),
         frac_1 = ifelse(out_tot >= 4, 1 - frac_5 - frac_3, frac_1))
         !!!!!!

