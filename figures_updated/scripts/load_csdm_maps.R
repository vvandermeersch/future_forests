
# Fitness change
plan(multisession, workers = 1)
sim_fitness <- lapply(years, function(year){
  
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
names(sim_fitness) <- years
plan(sequential);gc()

# Model agreement on sign of fitness change
plan(multisession, workers = 1)
sim_agreement <- lapply(years, function(year){
  cat(year)
  
  wdw <- c((year-10),(year+10))
  
  .sign <- future_lapply(models, function(c){
    
    .sign <- lapply(gcms, function(m){
      
      sim_dir <- file.path(wd, "data", "simulations", species, c, m)
      
      ref <- rast(readRDS(file.path(sim_dir, paste0("historical", ".rds")))[c(2,1,3)])
      
      fitness <- rast(readRDS(file.path(sim_dir, scenario, paste0(wdw[1], "_", wdw[2], ".rds")))[c(2,1,3)])
      
      change <- fitness-ref
      sign <- ifel(change < 0, -1, 1)
      
      return(sign)
      
    }) %>% rast() %>% wrap()
    
    return(.sign)
  })
  
  sign <- lapply(.sign, unwrap) %>% rast()
  
  neg <- ifel(sign < 0, 1, 0) %>% sum()/nlyr(sign)
  neg <- ifel(neg < 0.8, 0, 1)
  
  pos <- ifel(sign > 0, 1, 0) %>% sum()/nlyr(sign)
  pos <- ifel(pos < 0.8, 0, 1)
  
  agreement <- pos + neg
  
  return(agreement)
  
}) %>% rast()
names(sim_agreement) <- years
plan(sequential);gc()

# Distribution
plan(multisession, workers = 1)
sim_distribution <- lapply(years, function(year){
  
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
names(sim_distribution) <- years
plan(sequential);gc() 
