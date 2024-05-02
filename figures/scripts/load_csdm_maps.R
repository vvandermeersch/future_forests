
# Fitness change
plan(multisession, workers = 1)
sim_fitness <- lapply(c(2030, 2060, 2090), function(year){
  
  wdw <- c((year-10), (year+10))
  
  .fitness <- future_lapply(models, function(c){
    
    .fitness <- lapply(gcms, function(m){
      
      sim_dir <- file.path(wd, "data", "simulations", species, c, m)
      
      ref <- rast(readRDS(file.path(sim_dir, paste0("historical", ".rds")))[c(2,1,3)])
      
      fitness <- rast(readRDS(file.path(sim_dir, scenario, paste0(wdw[1], "_", wdw[2], ".rds")))[c(2,1,3)])
      
      change <- (fitness-ref)
      return(change)
      
    }) %>% rast() %>% mean() %>% wrap()
    
    return(.fitness)
  }) 
  
  fitness <- lapply(.fitness, unwrap) %>% rast() %>% mean()
  
  return(fitness)
  
}) %>% rast()
names(sim_fitness) <- c(2030, 2060, 2090)
plan(sequential);gc()

# Distribution
plan(multisession, workers = 1)
sim_distribution <- lapply(c(2030, 2060, 2090), function(year){
  
  wdw <- c((year-10),(year+10))
  
  .dist <- future_lapply(models, function(c){
    
    .dist <- lapply(gcms, function(m){
      
      sim_dir <- file.path(wd, "data", "simulations", species, c, m)
      ths <- readRDS(file.path(wd, "data", "fit", m, species, paste0(c, ".rds")))$best_threshold
      
      fitness <- rast(readRDS(file.path(sim_dir, scenario, paste0(wdw[1], "_", wdw[2], ".rds")))[c(2,1,3)])
      dist <- ifel(fitness < ths, 0, 1)
      
      return(dist)
    }) %>% rast() %>% sum() %>% wrap()
    
    return(.dist)
  })
  
  dist <- lapply(.dist, unwrap) %>% rast() %>% sum()
  
  return(dist)
  
}) %>% rast()
names(sim_distribution) <- c(2030, 2060, 2090)
plan(sequential);gc() 
