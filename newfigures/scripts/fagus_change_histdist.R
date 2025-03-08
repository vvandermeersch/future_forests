
species <- 'fagus_sylvatica'
year <- 2090

baseline <- 1970:2000

# Compute Fagus sylvatica suitability change in its historical distribution


scenarios <- c("ssp245", "ssp585")
gcms <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")

# Fitness change according to mechanistic models
nsim <- 5
breakseq <- seq(0, nsim, 1)
calibrations <- 'expert'
wdw <- c((year-10):(year+10))
fit_change <- lapply(scenarios, function(s){
  
  fit_change <- lapply(gcms, function(m){
    
    fit_change <- lapply(calibrations, function(c){
      
      suit <- rast(file.path(wd, "data", "processed", species, "suit", paste0(c, ".tif")))
      
      # reference period
      indices <- which(names(suit) == paste0(m, "_hist") & time(suit, format ="years") %in% baseline)
      ref <- mean(subset(suit,indices))
      ths <- readRDS(file.path(wd, "data", "fit", m, species, paste0(c, ".rds")))$best_threshold
      dist <- ifel(ref < ths, NA, 1) # historical distribution
      
      # future fitness 
      indices <- which(names(suit) == paste0(m, "_", s) & time(suit, format ="years") %in% wdw)
      fitness <- mask(mean(subset(suit,indices)), dist)
      
      # perc. loss of surface
      distloss <-  ifel(fitness < ths, 0, NA)
      percloss <- length(values(distloss, na.rm = T))/length(values(dist, na.rm = T))*100
      
      change <- (fitness-ref) %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) 
      meanchange <- global(change, mean, na.rm = T)
      
      return(data.frame(meanchange = meanchange, percloss = percloss))
    })
    
    fit_change <- as.data.frame(do.call(rbind, fit_change))
    # average across calibrations, for one GCM
    
    return(data.frame(ssp = s, gcm = m, fit_change = colMeans(fit_change['mean']), perc_loss = colMeans(fit_change['percloss'])))
  }) 
  
  fit_change <- as.data.frame(do.call(rbind, fit_change))
  return(fit_change)
  
})
fit_change_epbm <- as.data.frame(do.call(rbind, fit_change))
fit_change_epbm$type <- 'expert'
fit_change_epbm$data <- 'exp'

# Fitness change according to hybrid models
nsim <- 50
breakseq <- seq(0, nsim, 10)
calibrations <- (paste0("subset",rep(1:2, each = 5),"_rep", 1:5))
ncores <- 10
wdw <- c((year-10):(year+10))
plan(multisession, workers = ncores)
fit_change <- lapply(scenarios, function(s){
  
  fit_change <- lapply(gcms, function(m){
    
    fit_change <- future_lapply(calibrations, function(c){
      
      suit <- rast(file.path(wd, "data", "processed", species, "suit", paste0(c, ".tif")))
      
      # reference period
      indices <- which(names(suit) == paste0(m, "_hist") & time(suit, format ="years") %in% baseline)
      ref <- mean(subset(suit,indices))
      ths <- readRDS(file.path(wd, "data", "fit", m, species, paste0(c, ".rds")))$best_threshold
      dist <- ifel(ref < ths, NA, 1) # historical distribution
      
      # future fitness
      indices <- which(names(suit) == paste0(m, "_", s) & time(suit, format ="years") %in% wdw)
      fitness <- mask(mean(subset(suit,indices)), dist)
      
      # perc. loss of surface
      distloss <-  ifel(fitness < ths, 0, NA)
      percloss <- length(values(distloss, na.rm = T))/length(values(dist, na.rm = T))*100
      
      change <- (fitness-ref) %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) 
      meanchange <- global(change, mean, na.rm = T)
      
      return(data.frame(meanchange = meanchange, percloss = percloss))
    })
    
    fit_change <- as.data.frame(do.call(rbind, fit_change))
    # average across calibrations, for one GCM
    
    return(data.frame(ssp = s, gcm = m, fit_change = colMeans(fit_change['mean']), perc_loss = colMeans(fit_change['percloss'])))
  }) 
  
  fit_change <- as.data.frame(do.call(rbind, fit_change))
  return(fit_change)
  
})
plan(sequential)
fit_change_fpbm <- as.data.frame(do.call(rbind, fit_change))
fit_change_fpbm$type <- 'fitted'
fit_change_fpbm$data <- 'dist'

# Fitness change according to correlative models
nsim <- 20
breakseq <- seq(0, nsim, 1)
models <- c("lasso_glm", "random_forest", "gam", "brt")
wdw <- c((year-10), (year+10))
fit_change <- lapply(scenarios, function(s){
  
  fit_change <- lapply(gcms, function(m){
    
    fit_change <- lapply(models, function(c){
      
      sim_dir <- file.path(wd, "data", "simulations", species, c, m)
      
      # reference period
      ref <- rast(readRDS(file.path(sim_dir, paste0("historical", ".rds")))[c(2,1,3)])
      ths <- readRDS(file.path(wd, "data", "fit", m, species, paste0(c, ".rds")))$best_threshold
      dist <- ifel(ref < ths, NA, 1) # historical distribution
      
      # future fitness
      fitness <- mask(rast(readRDS(file.path(sim_dir, scenario, paste0(wdw[1], "_", wdw[2], ".rds")))[c(2,1,3)]), dist)
      
      # perc. loss of surface
      distloss <-  ifel(fitness < ths, 0, NA)
      percloss <- length(values(distloss, na.rm = T))/length(values(dist, na.rm = T))*100
      
      change <- (fitness-ref) %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) 
      meanchange <- global(change, mean, na.rm = T)
      
      return(data.frame(meanchange = meanchange, percloss = percloss))
    })
    
    fit_change <- as.data.frame(do.call(rbind, fit_change))
    # average across calibrations, for one GCM
    
    return(data.frame(ssp = s, gcm = m, fit_change = colMeans(fit_change['mean']), perc_loss = colMeans(fit_change['percloss'])))
  }) 
  
  fit_change <- as.data.frame(do.call(rbind, fit_change))
  return(fit_change)
  
})
fit_change_csdm <- as.data.frame(do.call(rbind, fit_change))
fit_change_csdm$type <- 'correlative'
fit_change_csdm$data <- 'dist'

fit_change <- rbind(fit_change_csdm, fit_change_fpbm, fit_change_epbm)

fit_change %>% 
  dplyr::group_by(ssp, data) %>%
  dplyr::summarise(avgchange = mean(fit_change), sdchange = sd(fit_change),
                   avgloss = mean(perc_loss), sdloss = sd(perc_loss))

fit_change %>% 
  dplyr::group_by(ssp) %>%
  dplyr::summarise(avgchange = mean(fit_change), sdchange = sd(fit_change),
                   avgloss = mean(perc_loss), sdloss = mean(perc_loss))
