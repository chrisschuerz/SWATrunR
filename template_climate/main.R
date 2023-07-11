setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(SWATrunR)
library(tidyverse)

n_thread <- 2

test_path <-"test/TxtInOut"
climate_set <- list.dirs(path = "test/cli3/cli3", full.names = TRUE, recursive = TRUE) %>%
  .[grepl('N|E|H',.)]
#
debug(run_swatplus)
options(error=recover)
res <- run_swatplus(project_path = test_path,
                    output = list(q = define_output('channel_sd', 'flo_out', 52),
                                  et = define_output("basin_wb", "et", 1)),
                    climate_set = climate_set,
                    output_interval = "y",
                    years_skip = 3,
                    n_thread = n_thread,
                    keep_folder = TRUE)

res <- run_swatplus(project_path = test_path,
                    output = list(precip = define_output("basin_wb", "precip", 1),
                                  snofall = define_output("basin_wb", "snofall", 1),
                                  snomlt = define_output("basin_wb", "snomlt", 1),
                                  perc = define_output("basin_wb", "perc", 1),
                                  et = define_output("basin_wb", "et", 1),
                                  ecanopy = define_output("basin_wb", "ecanopy", 1),
                                  eplant = define_output("basin_wb", "eplant", 1),
                                  esoil = define_output("basin_wb", "esoil", 1),
                                  sw_ave = define_output("basin_wb", "sw_ave", 1),
                                  pet = define_output("basin_wb", "pet", 1),
                                  qtile = define_output("basin_wb", "qtile", 1),
                                  surq_cha = define_output("basin_wb", "surq_cha", 1),
                                  latq_cha = define_output("basin_wb", "latq_cha", 1),
                                  q = define_output('channel_sd', 'flo_out', 52),
                                  latq = define_output("basin_wb", "latq", 1),
                                  flo = define_output("aquifer", "flo", 1),
                                  dep_wt = define_output("aquifer", "dep_wt", 1),
                                  rchrg = define_output("aquifer", "rchrg", 1),
                                  revap = define_output("aquifer", "revap", 1),
                                  seep = define_output("aquifer", "seep", 1)),
                    climate_set = climate_set,
                    output_interval = "a",
                    years_skip = 3,
                    n_thread = n_thread,
                    keep_folder = TRUE)

all(!grepl('yld', output$expr, fixed = TRUE))
if(all(!grepl('yld', output$expr, fixed = TRUE))){
  print("Y")
} else {
  print("N")
}
