pro_path <- "C:/TxtInOut_2aqu/TxtInOut_2aqu"

parameter <- tibble::tibble("cn2.bsn|change = pctchg" = runif(10, -25, 10),
               "esco.bsn|change = absval" = runif(10, 0.1, 0.85),
               "epco.bsn|change = absval" = runif(10, 0.1, 0.9))

test <- run_swatplus(project_path = pro_path, parameter = parameter, output = define_output(file = "channel", variable = "floout", unit = 18),
             start_date = "2009-01-01", end_date = "2010-12-31", years_skip = 1)

library(tidyverse)

test$simulation$floout %>%
  gather(key = "run", value = "discharge", - date) %>%
  ggplot(data = .) +
  geom_line(aes(x = date, y = discharge, col = run))
