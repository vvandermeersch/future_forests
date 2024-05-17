
#--------------------------------#
# Load 21-year simulations, CSDM #
#--------------------------------#

# get country extent
world_map <- ne_countries(scale="medium",returnclass = 'sf')
.country_map <- world_map %>% 
  filter(sovereignt %in% country) %>%
  sf::st_crop(sf::st_bbox(c(xmin = -12, xmax = 45, ymax = 71, ymin = 32), crs = sf::st_crs(4326))) %>%
  vect() %>% wrap()

# number of SSPs/GCMs/SDMs
Ngcm <- length(gcms)
Nssp <- length(scenarios)
Nsdm <- length(models)


plan(multisession, workers = Nsdm)
simulations_csdm <- foreach(c = 1:Nsdm, .combine=rbind) %dofuture% {
  
  ci <- models[c]
  
  country_map <- unwrap(.country_map)
  
  sim_temp <- data.frame()
  for(m in 1:Ngcm){
    
    mi <- gcms[m]
    
    sim_dir <- file.path(wd, "data", "simulations", species, ci, mi)
    ref <- rast(readRDS(file.path(sim_dir, paste0("historical", ".rds")))[c(2,1,3)])
    crs(ref) <- "EPSG:4326"
    ref <- ref %>% crop(country_map, mask = T)
    ref <- as.numeric(global(ref, mean, na.rm = TRUE))
    
    for(s in 1:Nssp){
      
      si <- scenarios[s]
      
      y <- rast(lapply(c(1980:2090), function(yr) rast(readRDS(file.path(sim_dir, si, paste0(yr-10, "_", yr+10, ".rds")))[c(2,1,3)]))) 
      crs(y) <- "EPSG:4326"
      y <- y %>% crop(country_map, mask = T)

      y <- unlist(global(y, mean, na.rm = TRUE))
      x <- c(1980:2090)
      x <- x - (min(x)) + 1
      
      # relative change
      y <- (y-ref)/ref*100
      
      # plot(y~x) ; lines(smy~x)
      
      sim_temp <- rbind(
        sim_temp,
        data.frame(year = c(1980:2090),
                   cal = ci, gcm = mi, ssp = si, y, ref = ref)
      )
      
    }
  }
  sim_temp
}
plan(sequential);gc()
