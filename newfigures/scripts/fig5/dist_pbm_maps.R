
# historical distribution
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
hist_dist <- lapply(.hist_dist, unwrap) %>% rast() %>% sum()

plan(sequential)
names(hist_dist) <- "historical"

dist <- c(hist_dist, sim_distribution) %>% crop(ext(-12.05, 40.05, 34.65, 71.15)) %>% project("EPSG:3035")
dist <- ifel(dist >= ths_ag_dist, 1, 0) 

cstdist <- ifel(dist$historical == 1 & dist$`2090` == 1, 1, 0)
maskd <- ifel(cstdist == 1, 1, NA)

ctr <- dist$historical
ctr <- ifel(is.na(ctr), 0, 1) %>% as.polygons() 

chgdist <- dist$`2090`-dist$historical
chgdist <- ifel(chgdist == 0, NA, chgdist)
nochg <- ifel(cstdist == 1, 0, NA)
chgdist <- sum(chgdist,nochg, na.rm = TRUE) %>%
  mutate(`2090` = factor(`2090`))

sim_fitness_rp <- sim_fitness %>% crop(ext(-12.05, 40.05, 34.65, 71.15)) %>% project("EPSG:3035")
sim_agreement_rp <- sim_agreement %>% crop(ext(-12.05, 40.05, 34.65, 71.15)) %>% project("EPSG:3035")

decsuit <- ifel(sim_fitness_rp$`2090` < -0.1 & sim_agreement_rp$`2090` ==1, -1, 
                ifel(sim_fitness_rp$`2090` > 0.1 & sim_agreement_rp$`2090` ==1, 1, NA)) %>% 
  mask(maskd) %>% 
  mutate(`2090` = factor(`2090`)) 

dist2090r <- c(cstdist, chgdist, decsuit)
saveRDS(dist2090r, file = file.path(wd,"figures_cleaned","data","maps", paste0(species, "_", type, "_2090_dist_", scenario, ".rds")))




