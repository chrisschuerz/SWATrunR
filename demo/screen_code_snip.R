SWATplusR:::screen(project_path = "D:/TxtInOut_2aqu_softcal",
                   parameter = tibble("cn2.hru|change = pctchg" = c(-0.3, 0.1),
                                      "dep_imp.hru|change = absval" = c(0,10)),
                   wb_fraction = c(srr = 0.1, lfr = 0.02, pcr = 0.1, etr = 0.53, tfr = 0.2), n_thread = 4)
