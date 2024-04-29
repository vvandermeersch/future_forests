library(terra)
library(dplyr)
library(rnaturalearth)
library(data.table)
library(ggplot2)
library(future.apply)
source(file.path("C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations", "scripts", "functions", "read_mean_outputvalue.R"))

# Get France extent
world_map <- ne_countries(scale="medium",returnclass = 'sf')
france_map <- world_map %>% 
  filter(sovereignt %in% "France") %>%
  sf::st_crop(sf::st_bbox(c(xmin = -12, xmax = 45, ymax = 71, ymin = 32), crs = sf::st_crs(4326))) %>%
  vect()

# Load simulations
dir <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
ths_dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/fitted/fagus_sylvatica"
calibrations <- c(paste0("subset2_rep", 1:10), paste0("subset8_rep", 1:10))
calibrations <- c("expert", "subset4_rep1", "subset5_rep4", "subset3_rep8", "subset1_rep7")

thresholds <- c(0.162,
  sapply(calibrations[2:5], function(c) readRDS(paste0(ths_dir, "/", "cmaes_fit_", c, ".rds"))$best_threshold))

gcms <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
scenarios <- c("ssp245", "ssp585")

years <- 2020:2100

window <- 11
r <- embed(years, window)[, c(window,1)]
yr_windows <- split(r, row(r))

.france_map <- wrap(france_map)
plan(multisession, workers = 24)
simulations <- as.data.frame(do.call(rbind, future_lapply(yr_windows, function(yr){
  france_map <- vect(.france_map)

  # Load simulations
  simulations <- as.data.frame(do.call(rbind, lapply(gcms, function(m){
    sim <- as.data.frame(do.call(rbind, lapply(scenarios, function(s){
      sim <- as.data.frame(do.call(rbind, lapply(1:length(calibrations), function(i){
        c <- calibrations[i]
        ths <- thresholds[i]
        sim_dir <- file.path(dir, "data", "simulations", "future", m, s)
        output <- read_mean_outputvalue(file.path(sim_dir, c), 
                                        years = c(yr[1]:yr[2]), model = "PHENOFIT", output_var = "Fitness")
        output <- rast(output[,c(2,1,3)]) %>%
          crop(france_map, mask=T)
        output <- ifel(output < as.numeric(ths), 0, 1)
        
        return(c(ncl = ncell(output[output == 1]), calibration = c))
      })))
      sim$scenario <- s
      return(sim)
    })))
    sim$gcm <- m
    return(sim)
  })))
  simulations$year <- (yr[1]+yr[2])/2
  return(simulations)
})))
plan(sequential)
gc()

ggplot(data = simulations, aes(x = year, y = as.numeric(ncl), 
                               col = scenario, group = paste0(gcm, scenario, calibration))) +
  geom_line(alpha = 0.1) +
  stat_summary(aes(group = scenario), color = "white", fun.y=mean, geom="line", linewidth = 1.8) + 
  stat_summary(aes(group = scenario), fun.y=mean, geom="line", linewidth = 0.7) + 
  theme_minimal() +
  ylab("Fagus cells occupied")



