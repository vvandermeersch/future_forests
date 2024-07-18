
# Fitness change
plan(multisession, workers = ncores)
sim_fitness <- lapply(years, function(year){
  
  wdw <- c((year-10):(year+10))
  
  .fitness <- future_lapply(calibrations, function(c){
    
    suit <- rast(file.path(wd, "data", "processed", species, "suit", paste0(c, ".tif")))
    
    .fitness <- lapply(gcms, function(m){
      
      indices <- which(names(suit) == paste0(m, "_hist") & time(suit, format ="years") %in% baseline)
      ref <- mean(subset(suit,indices))
      
      indices <- which(names(suit) == paste0(m, "_", scenario) & time(suit, format ="years") %in% wdw)
      fitness <- mean(subset(suit,indices))
      
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
saveRDS(sim_fitness, file = file.path(wd,"figures_updated","data","maps", species, type, paste0("fitness_", scenario, ".rds")))

# Model agreement on sign of fitness change
plan(multisession, workers = ncores)
sim_agreement <- lapply(years, function(year){
  cat(year)
  
  wdw <- c((year-10):(year+10))
  
  .sign <- future_lapply(calibrations, function(c){
    
    suit <- rast(file.path(wd, "data", "processed", species, "suit", paste0(c, ".tif")))
    
    .sign <- lapply(gcms, function(m){
      
      indices <- which(names(suit) == paste0(m, "_hist") & time(suit, format ="years") %in% baseline)
      ref <- mean(subset(suit,indices))
      
      indices <- which(names(suit) == paste0(m, "_", scenario) & time(suit, format ="years") %in% wdw)
      fitness <- mean(subset(suit,indices))
      
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
saveRDS(sim_agreement, file = file.path(wd,"figures_updated","data","maps", species, type, paste0("agreement_", scenario, ".rds")))

# Distribution
plan(multisession, workers = ncores)
sim_distribution <- lapply(years, function(year){
  
  wdw <- c((year-10):(year+10))
  
  .dist <- future_lapply(calibrations, function(c){
    
    suit <- rast(file.path(wd, "data", "processed", species, "suit", paste0(c, ".tif")))
    
    .dist <- lapply(gcms, function(m){
      
      ths <- readRDS(file.path(wd, "data", "fit", m, species, paste0(c, ".rds")))$best_threshold
      
      indices <- which(names(suit) == paste0(m, "_", scenario) & time(suit, format ="years") %in% wdw)
      
      fitness <- mean(subset(suit,indices))
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
saveRDS(sim_distribution, file = file.path(wd,"figures_updated","data","maps", species, type, paste0("distribution_", scenario, ".rds")))
