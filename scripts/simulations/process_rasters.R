
# Get France extent
world_map <- ne_countries(scale="medium",returnclass = 'sf')
france_map <- world_map %>% 
  filter(sovereignt %in% "France") %>%
  sf::st_crop(sf::st_bbox(c(xmin = -12, xmax = 45, ymax = 71, ymin = 32), crs = sf::st_crs(4326))) %>%
  vect()

# Load thresholds
ths_dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/fitted/fagus_sylvatica"
thresholds <- c(sapply(calibrations, 
                       function(c) readRDS(paste0(ths_dir, "/", "cmaes_fit_", c, ".rds"))$best_threshold))

# Process simulations
baseline <- 1970:2000
projections <- 2000:2100
window <- 11

fagus_france <- lapply(calibrations, function(c){
  
  sim <- rast(simulations["subset2_rep1"]) %>%
    crop(france_map, mask=T)
  
  # Loop on GCMs
  proj <- as.data.frame(do.call(rbind, lapply(gcms, function(m){
    ths <- readRDS(paste0(ths_dir, "/", "cmaes_fit_", c, ".rds"))$best_threshold
    
    # baseline
    indices <- which(time(output, format ="years") %in% baseline & names(output) == paste0(m, "_hist"))
    bas <- mean(subset(sim, indices)) # mean fitness over the period
    bas <- ifel(bas < as.numeric(ths), 0, 1) # apply threshold
    bas <- ncell(bas[bas == 1])
    
    # loop on years
    r <- embed(projections, window)[, c(window,1)]
    yr_windows <- split(r, row(r)) # create temporal windows
    proj <- as.data.frame(do.call(rbind, lapply(yr_windows, function(yr){
      # loop on scenarios
      proj <- as.data.frame(do.call(rbind, lapply(scenarios, function(s){
        indices <- which(time(output, format ="years") %in% c(yr[1]:yr[2]) & names(output) == paste0(m, "_", s))
        proj <- mean(subset(sim, indices)) # mean fitness over the period
        proj <- ifel(proj < as.numeric(ths), 0, 1) # apply threshold
        return(c(ncl = ncell(proj[proj == 1]), ssp = s))
      })))
      proj$year <- (yr[1]+yr[2])/2
      return(proj)
    })))
    proj$gcm <- m
    proj$ref <- bas
    return(proj)
  })))
  
  proj$cal <- c
  return(proj)
  
})