
#-------------------------#
# Load yearly simulations #
#-------------------------#

# get country extent
world_map <- ne_countries(scale="medium",returnclass = 'sf')
.country_map <- world_map %>% 
  filter(sovereignt %in% country) %>%
  sf::st_crop(sf::st_bbox(c(xmin = -12, xmax = 45, ymax = 71, ymin = 32), crs = sf::st_crs(4326))) %>%
  vect() %>% wrap()

# number of SSPs/GCMs/SDMs
Ngcm <- length(gcms)
Nssp <- length(scenarios)
Nsdm <- length(calibrations)

# degree for polynomial fitting (if needed)
ndeg_poly <- 4

plan(multisession, workers = ncores)
simulations_pbm <- foreach(c = 1:Nsdm, .combine=rbind) %dofuture% {
  
  ci <- calibrations[c]
  
  country_map <- unwrap(.country_map)
  suit <- rast(file.path(wd, "data", "processed", species, "suit", paste0(ci, ".tif"))) %>% 
    crop(country_map, mask = T)
  
  sim_temp <- data.frame()
  for(m in 1:Ngcm){
    
    mi <- gcms[m]
    indices <- which(names(suit) == paste0(mi, "_hist") & time(suit, format ="years") %in% baseline)
    ref <- as.numeric(global(mean(subset(suit,indices)), mean, na.rm = TRUE))
    
    for(s in 1:Nssp){
      
      si <- scenarios[s]
      
      indices_hist <- which(names(suit) == paste0(mi, "_hist") & time(suit, format ="years") %in% c(1970:2010))
      indices <- c(indices_hist, which(names(suit) == paste0(mi, "_", si)))
      y <- unlist(global(subset(suit,indices), mean, na.rm = TRUE))
      x <- lubridate::year(time(subset(suit,indices)))
      x <- x - (min(x)) + 1
      
      # relative change
      y <- (y-ref)/ref*100
      
      # plot(y~x) ; lines(smy~x)
      
      sim_temp <- rbind(
        sim_temp,
        data.frame(year = lubridate::year(time(subset(suit,indices))),
                   cal = ci, gcm = mi, ssp = si, y, ref = ref)
      )
      
    }
  }
  sim_temp
}
plan(sequential);gc()
