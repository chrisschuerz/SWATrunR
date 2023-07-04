# Parameter sampling settings --------------------------------------------------

##Parameter bounds
par_bound <- tibble('epco.hru  | change = absval' = c(0, 1),
                    'tmp_opt.plt | change = abschg' = c(-4, 4),
                    'tmp_base.plt | change = absval' = c(0, 7),
                    'harv_idx.plt | change = absval' = c(0.1, 0.95),
                    'lai_pot.plt | change = absval' = c(1, 7),
                    'bm_e.plt | change = relchg' = c(-0.5, 0.7))

##If parameter changes are done in different files than calibration.cal, files
##to changed is provided
parameter_files <-  tibble(parameter = c('tmp_opt', 'tmp_base', 'harv_idx',
                                         'lai_pot', 'bm_e'),
                           file = c('plants.plt', 'plants.plt', 'plants.plt',
                                    'plants.plt', 'plants.plt'))

n_sample <- 100

# Model run settings -----------------------------------------------------------

##Path to model files
mod_path <- '../test/TxtInOut'
##Dates
st_date <- 20030101
en_date <- 20121231
st_prt_date <- 20060101
n_thread <- 10

#Output assessment data --------------------------------------------------------

##Crop observation data (units are dry t/year)
crops_obs <- data.frame(crop = c("wwht", "corn", "barl", "trit",
                                 "canp", "alfa", "fesc", "sgbt",
                                 "mint", "onio", "crrt", "lett"),
                        value = c(6.5, 7.2, 4.9, 6.2,
                                  3, 5.3, 5, 14.4,
                                  1.5, 2.5, 9.8, 4.1))

#Catchment area
cmt_area <- 148000000 #m2
# Average annual precipitation from station data
pcp_avann_obs <- 508 #mm
# Average annual discharge in mm from station data
q_avann_obs <- 60 #mm
