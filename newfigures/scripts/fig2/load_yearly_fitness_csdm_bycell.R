
#-----------------------------------------#
# Load 21-year simulations, CSDM, by cell #
#-----------------------------------------#

# for 2080-2100

# number of SSPs/GCMs/SDMs
Ngcm <- length(gcms)
Nssp <- length(scenarios)
Nsdm <- length(csdms)

plan(multisession, workers = Nsdm)
simulations_csdm <- foreach(c = 1:Nsdm, .combine=rbind) %dofuture% {
  
  ci <- csdms[c]
  
  sims <- data.frame()
  for(m in 1:Ngcm){
    
    mi <- gcms[m]
    
    sim_dir <- file.path(wd, "data", "simulations", species, ci, mi)
    ref <- rast(readRDS(file.path(sim_dir, paste0("historical", ".rds")))[c(2,1,3)])
    crs(ref) <- "EPSG:4326"
    
    for(s in 1:Nssp){
      
      si <- scenarios[s]
      
      y <- rast(lapply(c(2090), function(yr) rast(readRDS(file.path(sim_dir, si, paste0(yr-10, "_", yr+10, ".rds")))[c(2,1,3)]))) 
      crs(y) <- "EPSG:4326"
      
      # absolute change
      y <- (y-ref)
      
      # plot(y~x) ; lines(smy~x)
      
      sim_temp <- as.data.frame(y, xy = TRUE)
      names(sim_temp) <- c("x", "y", "mean")
      sim_temp$cal <- ci
      sim_temp$gcm <- mi
      sim_temp$ssp <- si
      sim_temp$id_cell <- 1:nrow(sim_temp)
      sim_temp$method <- "correlative"
      
      sims <- rbind(
        sims,
        sim_temp
      )
      
    }
  }
  sims
}
plan(sequential);gc()
