
# Fitness change
sim_fitness <- lapply(years, function(year){
  
  wdw <- c((year-10):(year+10))
  
  .fitness <- lapply("expert", function(c){
    
    suit <- rast(file.path(wd, "data", "processed", species, "suit", paste0(c, ".tif")))
    
    .fitness <- lapply(gcms, function(m){
      
      indices <- which(names(suit) == paste0(m, "_hist") & time(suit, format ="years") %in% baseline)
      ref <- mean(subset(suit,indices))
      
      indices <- which(names(suit) == paste0(m, "_", scenario) & time(suit, format ="years") %in% wdw)
      fitness <- mean(subset(suit,indices))
      
      change <- fitness-ref
      return(change)
      
    }) %>% rast() %>% mean() %>% wrap()
    
    return(.fitness)
  }) 
  
  fitness <- lapply(.fitness, unwrap) %>% rast() %>% mean()
  
  return(fitness)
  
}) %>% rast()
names(sim_fitness) <- years
saveRDS(sim_fitness, file = file.path(wd,"figures_updated","data","maps", species, "expert", paste0("fitness_", scenario, ".rds")))

# Distribution
sim_distribution <- lapply(years, function(year){
  
  wdw <- c((year-10):(year+10))
  
  .dist <- lapply("expert", function(c){
    
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
saveRDS(sim_distribution, file = file.path(wd,"figures_updated","data","maps", species, "expert", paste0("distribution_", scenario, ".rds")))
