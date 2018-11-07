library(tidyverse)
library(pasta)
library(SWATplusR)

proj_path <- "D:/SWATplus/LREW_varratio"



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
           frac_6 = ifelse(frac_6 > 0.99 & out_tot == 6, 0.99, frac_6),
           frac_6 = ifelse(frac_6 < 0.01 & out_tot == 6, 0.01, frac_6),
           frac_5 = frac_6,
           frac_4 = 1 - frac_6,
           frac_3 = ifelse((frac_1 + frac_3) > 0, (1 - frac_5)*frac_3/(frac_1 + frac_3) + rtu_val, frac_3),
           frac_3 = ifelse((frac_3 + frac_5) > 0.99, 0.99 - frac_5, frac_3),
           frac_3 = ifelse((frac_3 < 0.01) & !((frac_1 + frac_3) == 0), 0.01, frac_3),
           frac_1 = 1 - frac_5 - frac_3) %>%
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


run_swatplus(project_path = lrew_path, output = define_output("basin_wb", "flo_out", 1), run_path = "D:/SWATplus", keep_folder = TRUE, refresh = FALSE, n_thread = 1, years_skip = 1, start_date = "1999-01-01", end_date = "2000-12-31")


