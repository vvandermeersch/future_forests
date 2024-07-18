
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

# change in distribution - 2090 (fig 5)
ths_ag_dist <- 0.5*nsim
## historical distribution
plan(multisession, workers = 1)
.hist_dist <- future_lapply(models, function(c){
  
  dist <- lapply(gcms, function(m){
    
    sim_dir <- file.path(wd, "data", "simulations", species, c, m)
    ths <- readRDS(file.path(wd, "data", "fit", m, species, paste0(c, ".rds")))$best_threshold
    
    fitness <- rast(readRDS(file.path(sim_dir, paste0("historical", ".rds")))[c(2,1,3)])
    dist <- ifel(fitness < ths, 0, 1)
    
    return(dist)
  }) %>% rast() %>% sum()
  
  return(wrap(dist))
}) 
hist_dist <- lapply(.hist_dist, unwrap) %>% rast() %>% sum()

plan(sequential)
names(hist_dist) <- "historical"

dist <- c(hist_dist, sim_distribution) %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) 
crs(dist) <- "EPSG:4326"
dist <- dist %>% project("EPSG:3035")
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

crs(sim_fitness) <- "EPSG:4326"
crs(sim_agreement) <- "EPSG:4326"
sim_fitness_rp <- sim_fitness %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) %>% project("EPSG:3035")
sim_agreement_rp <- sim_agreement %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) %>% project("EPSG:3035")

decsuit <- ifel(sim_fitness_rp$`2090` < -0.1 & sim_agreement_rp$`2090` ==1, -1, 
                ifel(sim_fitness_rp$`2090` > 0.1 & sim_agreement_rp$`2090` ==1, 1, NA)) %>% 
  mask(maskd) %>% 
  mutate(`2090` = factor(`2090`)) 

sim_dist_2090 <- c(cstdist, chgdist, decsuit)
