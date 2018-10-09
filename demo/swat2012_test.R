library(SWATplusR)
library(SWATpasteR)
n_par <- 40
parameter <- tibble::tibble( "v__SFTMP.bsn" = 3.53348419846346*runif(n_par),
                "v__SMTMP.bsn" = 1.3663399904035*runif(n_par),
                "v__SMFMX.bsn" = 0.295715016390507*runif(n_par),
                "v__SMFMN.bsn" = 8.58199222432449*runif(n_par),
                "r__CNOP{[],5}.mgt" = -0.125933722419043*runif(n_par),
                "r__CNOP{[],6}.mgt" = -0.0787304956341783*runif(n_par))

parameter <- c("sft::SFTMP.bsn|change = abs" = 3.53348419846346,
               "smt::SMTMP.bsn|change = rep" = 1.3663399904035)

parameter <- c("gw_del::GW_DELAY.gw|change = abs" = 3.53348419846346)
,
               "smt::SMTMP.bsn|change = rep" = 1.3663399904035)
,
                     "v__SMTMP.bsn" = 1.3663399904035,
                     "v__SMFMX.bsn" = 0.295715016390507,
                     "v__SMFMN.bsn" = 8.58199222432449,
                     "r__CNOP{[],5}.mgt" = -0.125933722419043,
                     "r__CNOP{[],6}.mgt" = -0.078730495634178)

aa <- run_swat2012(project_path = "D:/UnLoadC3/00_SW_SWAT/model_struct/sb03_thru",
             output = list(flow = define_output(file = "rch", variable = "FLOW_OUT", unit = 1:2),
                           sw = define_output(file = "sub", variable = "SW", unit = 2)),
             parameter = parameter, n_thread = 4,
             keep_folder = F, output_interval = "d", start_date = "20050101", end_date = "20101225")

aa <- run_swat2012(project_path = "D:/UnLoadC3/00_SW_SWAT/model_struct/sb03_thru",
                   output = list(p_tot = define_output(file = "rch", variable = "TOT_P", unit = 1:2)),
                   keep_folder = T)

rch_file <- read.table("D:/UnLoadC3/00_SW_SWAT/model_struct/sb03_thru/.model_run/thread_1/output.rch",
                       skip = 9)
rch_file %>% filter((V4 > (quantile(V4, probs = 0.75, type = 3) - 300)) &
                    (V4 < (quantile(V4, probs = 0.75, type = 3) + 200))) %>%
  select(1:7) %>%
  summary()

run_swat2012("D:/UnLoadC3/00_SW_SWAT/model_struct/sb03_thru",
             output = list(flow =  define_output("rch", "FLOW_OUT", 1:2),
                           sw   =  define_output("sub", "SW", 1)))

