setwd("D:/Users/Svajunas/SWATrunR/template_climate")
library(SWATrunR)
library(tidyverse)

n_thread <- 15

test_path <-"../test/TxtInOut"

climate_set <- list.dirs(path = "../test/cli3", full.names = TRUE, recursive = TRUE) %>%
  .[grepl('/N|/E|/H',.)]
#
# debug(run_swatplus)
# options(error=recover)
res <- run_swatplus(project_path = test_path,
                    output = list(q = define_output('channel_sd', 'flo_out', 52),
                                  et = define_output("basin_wb", "et", 1)),
                    climate_set = climate_set,
                    output_interval = "y",
                    years_skip = 3,
                    n_thread = n_thread,
                    keep_folder = TRUE)
