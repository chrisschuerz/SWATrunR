##Loading required libraries and data
source("load.R", encoding = "UTF-8")
source("function.R", encoding = "UTF-8")

par_lhs <- sample_lhs(par_bound, n_sample)

# Average annual ET estimated from pcp and q
et_avann_est <- pcp_avann_obs - q_avann_obs
res <- run_swatplus(project_path = mod_path,
                    output = list(q = define_output('channel_sd', 'flo_out', 52),
                                  crop = define_output('basin_crop_yld', 'yld(t/ha)', 1),
                                  et = define_output("basin_wb", "et", 1)),
                    parameter = par_lhs,
                    parameter_files = parameter_files,
                    crop_filter = crops_obs$crops,
                    output_interval = "y",
                    start_date = st_date,
                    end_date = en_date,
                    start_date_print = st_prt_date,
                    n_thread = n_thread,
                    keep_folder = TRUE)

et_df <- get_par_values(res, "et")
et_fig <- plot_dot(et_df, "et", "Parameter", data.frame(parameter = "et", value = et_avann_est))

##Years skip
years_skip <- round(as.numeric((as.Date(as.character(st_prt_date), "%Y%m%d") - as.Date(as.character(st_date), "%Y%m%d"))/365), 0)

sim0 <- run_swat_verification(mod_path, outputs = c('wb', 'mgt', 'plt'), start_date = st_date, end_date = en_date, years_skip = years_skip, nostress = 0)
sim1 <- run_swat_verification(mod_path, outputs = c('wb', 'mgt', 'plt'), start_date = st_date, end_date = en_date, years_skip = years_skip, nostress = 1)
sim2 <- run_swat_verification(mod_path, outputs = c('wb', 'mgt', 'plt'), start_date = st_date, end_date = en_date, years_skip = years_skip, nostress = 2)

df <- res$simulation %>%
  bind_rows(., .id = "crop") %>%
  filter(str_detect(crop, 'crop')) %>%
  mutate(crop = gsub("^crop_*?_","",crop)) %>%
  select(-date) %>%
  group_by(crop) %>%
  summarise_all(mean) %>%
  pivot_longer(!crop, names_to = "run", values_to = "yield") %>%
  mutate(run = as.numeric(gsub("^run_*?_","",run))) %>%
  left_join(res$parameter$values %>%
              mutate(run = as.numeric(rownames(.))), by = c('run')) %>%
  select(-run) %>%
  left_join(crops_obs[c("crop", "yield_nb")], by = "crop") %>%
  mutate(yield_nb = ifelse(is.na(yield_nb), 1, yield_nb)) %>%
  mutate(yield = yield_nb*yield) %>%
  select(-yield_nb)%>%
  group_by(crop) %>%
  filter(sum(yield) > 0) %>%
  ungroup() %>%
  pivot_longer(!c("crop", "yield"), names_to = "Parameter", values_to = "Values")

crop_figs <- plot_pars(df, "yield", "Parameter", "crop", crops_obs)

swat_file <- list.files(mod_path, pattern = "\\.exe$")
rmarkdown::render("reports/report.qmd")

