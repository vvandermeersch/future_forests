
#--------------------------------#
# Load 21-year simulations, CSDM #
#--------------------------------#

# get country extent
.eco_map <- ecorast_wgs84 %>% filter(val == ecoregion) %>% wrap()

# number of SSPs/GCMs/SDMs
Ngcm <- length(gcms)
Nssp <- length(scenarios)
Nsdm <- length(csdms)


plan(multisession, workers = Nsdm)
simulations_csdm <- foreach(c = 1:Nsdm, .combine=rbind) %dofuture% {
  
  ci <- csdms[c]
  
  eco_map <- unwrap(.eco_map)
  
  sim_temp <- data.frame()
  for(m in 1:Ngcm){
    
    mi <- gcms[m]
    
    sim_dir <- file.path(wd, "data", "simulations", species, ci, mi)
    ref <- rast(readRDS(file.path(sim_dir, paste0("historical", ".rds")))[c(2,1,3)])
    crs(ref) <- "EPSG:4326"
    ref <- ref %>% crop(eco_map, mask = T)
    ref <- as.numeric(global(ref, mean, na.rm = TRUE))
    
    for(s in 1:Nssp){
      
      si <- scenarios[s]
      
      y <- rast(lapply(c(1980:2090), function(yr) rast(readRDS(file.path(sim_dir, si, paste0(yr-10, "_", yr+10, ".rds")))[c(2,1,3)]))) 
      crs(y) <- "EPSG:4326"
      y <- y %>% crop(eco_map, mask = T)

      y <- unlist(global(y, mean, na.rm = TRUE))
      x <- c(1980:2090)
      x <- x - (min(x)) + 1
      
      # relative change
      # y <- (y-ref)/ref*100
      
      # absolute change
      y <- (y-ref)
      
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
