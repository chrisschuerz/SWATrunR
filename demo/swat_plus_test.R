para <- tibble::tibble("cn2.hru|change = pctchg" = runif(8,-5,5),
               "alpha.gw|change = absval" = runif(8, 0, 1))
pro_pth <- "C:/180927_TxtInOut_2aqu"

aa <- run_swatplus(project_path = pro_pth,
                   output = list(q =  define_output(file = "channel",
                                                    variable = "floout",
                                                    unit = 18),
                                 et = define_output(file = "basin_wb",
                                                    variable = "et",
                                                    unit = 1)),
                   parameter = para,
                   start_date = "2007-01-01",
                   end_date = "2009-12-31",
                   output_interval = "d",
                   years_skip = 1,
                   n_thread = 4,
                   save_file = "test")
