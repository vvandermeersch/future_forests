
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
  
}) %>% rast() %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) 
names(sim_fitness) <- years
plan(sequential);gc()
saveRDS(sim_fitness, file = file.path(wd,"figures_cleaned","data","maps", paste0(species, "_", type, "_fitness_", scenario, ".rds")))

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
  
  neg <- ifel(sign < 0, 1, 0) %>% sum()/nsim
  neg <- ifel(neg < 0.8, 0, 1)
  
  pos <- ifel(sign > 0, 1, 0) %>% sum()/nsim
  pos <- ifel(pos < 0.8, 0, 1)
  
  agreement <- pos + neg
  
  return(agreement)
  
}) %>% rast() %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) 
names(sim_agreement) <- years
plan(sequential);gc()
saveRDS(sim_agreement, file = file.path(wd,"figures_cleaned","data","maps", paste0(species, "_", type, "_agreement_", scenario, ".rds")))

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
  
}) %>% rast() %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) 
names(sim_distribution) <- years
plan(sequential);gc() 
saveRDS(sim_distribution, file = file.path(wd,"newfigures","data","maps", paste0(species, "_", type, "_distribution_", scenario, ".rds")))

# change in distribution - 2090 (fig 5)
ths_ag_dist <- 0.5*nsim
## historical distribution
plan(multisession, workers = ncores)
.hist_dist <- future_lapply(calibrations, function(c){
  
  suit <- rast(file.path(wd, "data", "processed", species, "suit", paste0(c, ".tif")))
  
  dist <- lapply(gcms, function(m){
    
    ths <- readRDS(file.path(wd, "data", "fit", m, species, paste0(c, ".rds")))$best_threshold
    
    indices <- which(names(suit) == paste0(m, "_hist") & time(suit, format ="years") %in% c(1970:2000))
    
    fitness <- mean(subset(suit,indices))
    dist <- ifel(fitness < ths, 0, 1)
    
    return(dist)
  }) %>% rast() %>% sum()
  
  return(wrap(dist))
}) 
hist_dist <- lapply(.hist_dist, unwrap) %>% rast() %>% sum() %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) 

plan(sequential)
names(hist_dist) <- "historical"

dist <- c(hist_dist, sim_distribution) %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) %>% project("EPSG:3035")
dist <- ifel(dist > ths_ag_dist, 1, 0) 

cstdist <- ifel(dist$historical == 1 & dist$`2090` == 1, 1, 0)
maskd <- ifel(cstdist == 1, 1, NA)

ctr <- dist$historical
ctr <- ifel(is.na(ctr), 0, 1) %>% as.polygons() 

chgdist <- dist$`2090`-dist$historical
chgdist <- ifel(chgdist == 0, NA, chgdist)
nochg <- ifel(cstdist == 1, 0, NA)
chgdist <- sum(chgdist,nochg, na.rm = TRUE) %>%
  mutate(`2090` = factor(`2090`))

sim_fitness_rp <- sim_fitness %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) %>% project("EPSG:3035")
sim_agreement_rp <- sim_agreement %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) %>% project("EPSG:3035")

decsuit <- ifel(sim_fitness_rp$`2090` < -0.1 & sim_agreement_rp$`2090` ==1, -1, 
                ifel(sim_fitness_rp$`2090` > 0.1 & sim_agreement_rp$`2090` ==1, 1, NA)) %>% 
  mask(maskd) %>% 
  mutate(`2090` = factor(`2090`, levels = c(-2,-1,0,1,2))) 

sim_dist_2090 <- c(cstdist, chgdist, decsuit)
saveRDS(sim_dist_2090, file = file.path(wd,"newfigures","data","maps", paste0(species, "_", type, "_2090_dist_", scenario, ".rds")))


