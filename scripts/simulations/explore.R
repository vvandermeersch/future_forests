library(terra)
library(dplyr)
library(rnaturalearth)
library(data.table)
library(ggplot2)
source(file.path("C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations", "scripts", "functions", "read_mean_outputvalue.R"))

world_map <- ne_countries(scale="medium",returnclass = 'sf')
france_map <- world_map %>% 
  filter(sovereignt %in% "France") %>%
  sf::st_crop(sf::st_bbox(c(xmin = -12, xmax = 45, ymax = 71, ymin = 32), crs = sf::st_crs(4326))) %>%
  vect()

# current distribution
fagus_sylvatica_presabs <- readRDS("D:/species/processed/fagus_sylvatica/fagus_sylvatica_presabs.rds")
pres_rast <- rast(fagus_sylvatica_presabs) %>%
  crop(france_map, mask=T)
ncell(pres_rast[pres_rast == 1])/ncell(pres_rast[pres_rast >= 0]) # 87%
pres_mask <- ifel(pres_rast == 1, 1, NA)

# Phenofit, historical simulation
ths <- 0.785
# hist_simulation <- read_mean_outputvalue("D:/simulations/phenofit/present/expert/fagus_sylvatica/VVanderMeersch", 
#                                          years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")
hist_simulation <- read_mean_outputvalue("C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations/data/simulations/historical/subset4_rep1",
                                         years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")

hist_sim_rast <- rast(hist_simulation[,c(2,1,3)]) %>%
  crop(france_map, mask=T)
hist_sim_rast <- ifel(hist_sim_rast < ths, 0, 1)
hist_sim_rms <- mask(hist_sim_rast, pres_mask)
ncell(hist_sim_rms[hist_sim_rms == 1])/ncell(pres_mask[pres_mask == 1]) # 93% (forward), 91% (best backward)

# Decadal trends
ths <- c("subset4_rep1" = 0.785,
         "subset5_rep4" = 0.674)


years <- 2020:2100
window <- 11
r <- embed(years, window)[, c(window,1)]
yr_windows <- split(r, row(r))
dir <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"

gcms <- c("GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR", "UKESM1-0-LL")

.france_map <- wrap(france_map)
.pres_mask <- wrap(pres_mask)
plan(multisession, workers = 20)
simulations <- as.data.frame(do.call(rbind, future_lapply(yr_windows, function(yr){
  france_map <- vect(.france_map)
  pres_mask <- rast(.pres_mask)
  cat(paste0((yr[1]+yr[2])/2, "...\n"))
  
  # Load simulations
  simulations <- as.data.frame(do.call(rbind, lapply(gcms, function(m){
    sim <- as.data.frame(do.call(rbind, lapply(c("ssp245", "ssp585"), function(s){
      sim <- as.data.frame(do.call(rbind, lapply(calibrations , function(c){
        sim_dir <- file.path(dir, "data", "simulations", "future", m, s)
        output <- read_mean_outputvalue(file.path(sim_dir, c), 
                                        years = c(yr[1]:yr[2]), model = "PHENOFIT", output_var = "Fitness")
        output <- rast(output[,c(2,1,3)]) %>%
          crop(france_map, mask=T)
        output <- ifel(output < as.numeric(ths[c]), 0, 1)
        output <- mask(output, pres_mask)
        
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

ggplot(data = simulations, aes(x = year, y = as.numeric(ncl)/5551, 
                               col = scenario, group = paste0(gcm, scenario, calibration))) +
  geom_line(alpha = 0.2) +
  stat_summary(aes(group = scenario), fun.y=mean, geom="line") + 
  theme_minimal() +
  ylab("% of Fagus cells occupied")


test <- aov(ncl ~ year + scenario*gcm, data = simulations)
summary(test)
shapiro.test(residuals(test))
par(mfrow = c(2, 2)) # Split the plotting panel into a 2 x 2 grid
plot(test)


