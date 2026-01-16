
cont <- ecorast_wgs84 %>% filter(val == 'Continental')
med <- ecorast_wgs84 %>% filter(val == 'Mediterranean')
bor <- ecorast_wgs84 %>% filter(val == 'Boreal')

tmean_hist_cont <- c()
for(y in 1970:2000){
  tmean_y <- sapply(gcms, function(m){
    load(file.path("/home/victor/projects/future_forests/data/climate", ssp, m, paste0("bioclim_format/biovars_",y,".Rdata")))
    biovars <- crop(rast(biovars[,c('lon', 'lat', 'bio1')]), cont, mask = T)
    return(values(biovars, na.rm = T))
  })
  tmean_y <- rowMeans(tmean_y)
  tmean_hist_cont <- cbind(tmean_hist_cont, tmean_y)
}
tmean_hist_cont <- rowMeans(tmean_hist_cont)

tmean_20802100_cont <- c()
for(y in 2080:2100){
  tmean_y <- sapply(gcms, function(m){
    load(file.path("/home/victor/projects/future_forests/data/climate", ssp, m, paste0("bioclim_format/biovars_",y,".Rdata")))
    biovars <- crop(rast(biovars[,c('lon', 'lat', 'bio1')]), cont, mask = T)
    return(values(biovars, na.rm = T))
  })
  tmean_y <- rowMeans(tmean_y)
  tmean_20802100_cont <- cbind(tmean_20802100_cont, tmean_y)
}
tmean_20802100_cont <- rowMeans(tmean_20802100_cont)


mech <- rast('/home/victor/projects/future_forests/data/processed/fagus_sylvatica/suit/expert.tif')
idxs <- which((grepl(ssp,names(mech)) | grepl('hist',names(mech))) & time(mech, format ="years") %in% 1970:2100)
r <- mean(subset(mech,idxs))
names(r) <- 'mech_fitness'
mech_fitness <- as.data.frame(r)
mech_fitness <- mech_fitness$mech_fitness
mech_fitness <- (mech_fitness-min(mech_fitness))/(max(mech_fitness)-min(mech_fitness)) # scale to 0 - 1


idxs <- which((grepl(ssp,names(mech)) | grepl('hist',names(mech))) & time(mech, format ="years") %in% 1970:2000)
r <- mean(crop(subset(mech,idxs), cont, mask = T))
names(r) <- 'mech_fitness'
mech_fitness_cont_hist <- as.data.frame(r)
mech_fitness_cont_hist <- (mech_fitness_cont_hist$mech_fitness-min(mech_fitness))/(max(mech_fitness)-min(mech_fitness)) # scale to 0 - 1

idxs <- which((grepl(ssp,names(mech)) | grepl('hist',names(mech))) & time(mech, format ="years") %in% 2080:2100)
r <- mean(crop(subset(mech,idxs), cont, mask = T))
names(r) <- 'mech_fitness'
mech_fitness_cont_20802100 <- as.data.frame(r)
mech_fitness_cont_20802100 <- (mech_fitness_cont_20802100$mech_fitness-min(mech_fitness))/(max(mech_fitness)-min(mech_fitness)) # scale to 0 - 1



sdms <- c('lasso_glm', 'brt', 'gam', 'random_forest')
periods <- c('historical', '2000_2020', '2020_2040', '2040_2060', '2060_2080', '2080_2100')
csdm_fitness <- sapply(sdms, function(s){
  fit <- sapply(gcms, function(m){
    fit <- sapply(periods, function(p){
      if(p == 'historical'){
        proj <- readRDS(file.path("/home/victor/projects/future_forests/data/simulations/fagus_sylvatica",s, m, paste0(p, ".rds")))
        return(proj$value)
      }
      proj <- readRDS(file.path("/home/victor/projects/future_forests/data/simulations/fagus_sylvatica",s, m, "ssp245", paste0(p, ".rds")))
      return(proj$value)
    })
    fit <- rowMeans(fit)
  })
  fit <- rowMeans(fit)
  return(fit)
})
csdm_fitness <- rowMeans(csdm_fitness)
csdm_fitness <- (csdm_fitness-min(csdm_fitness))/(max(csdm_fitness)-min(csdm_fitness)) # scale to 0 - 1


periods <- c('historical')
csdm_fitness_cont_hist <- sapply(sdms, function(s){
  fit <- sapply(gcms, function(m){
    fit <- sapply(periods, function(p){
      print(p)
      if(p == 'historical'){
        proj <- readRDS(file.path("/home/victor/projects/future_forests/data/simulations/fagus_sylvatica",s, m, paste0(p, ".rds")))
        proj <- crop(rast(proj[,c(2,1,3)]), cont, mask = T)
        return(values(proj, na.rm = T))
      }
      proj <- readRDS(file.path("/home/victor/projects/future_forests/data/simulations/fagus_sylvatica",s, m, "ssp245", paste0(p, ".rds")))
      proj <- crop(rast(proj[,c(2,1,3)]), cont, mask = T)
      return(values(proj, na.rm = T))
    })
    fit <- rowMeans(fit)
  })
  fit <- rowMeans(fit)
  return(fit)
})
csdm_fitness_cont_hist <- rowMeans(csdm_fitness_cont_hist)
csdm_fitness_cont_hist <- (csdm_fitness_cont_hist-min(csdm_fitness))/(max(csdm_fitness)-min(csdm_fitness)) # scale to 0 - 1

periods <- c('2080_2100')
csdm_fitness_cont_20802100 <- sapply(sdms, function(s){
  fit <- sapply(gcms, function(m){
    fit <- sapply(periods, function(p){
      if(p == 'historical'){
        proj <- readRDS(file.path("/home/victor/projects/future_forests/data/simulations/fagus_sylvatica",s, m, paste0(p, ".rds")))
        proj <- crop(rast(proj[,c(2,1,3)]), cont, mask = T)
        return(values(proj, na.rm = T))
      }
      proj <- readRDS(file.path("/home/victor/projects/future_forests/data/simulations/fagus_sylvatica",s, m, "ssp245", paste0(p, ".rds")))
      proj <- crop(rast(proj[,c(2,1,3)]), cont, mask = T)
      return(values(proj, na.rm = T))
    })
    fit <- rowMeans(fit)
  })
  fit <- rowMeans(fit)
  return(fit)
})
csdm_fitness_cont_20802100 <- rowMeans(csdm_fitness_cont_20802100)
csdm_fitness_cont_20802100 <- (csdm_fitness_cont_20802100-min(csdm_fitness))/(max(csdm_fitness)-min(csdm_fitness)) # scale to 0 - 1







tmean_hist_med <- c()
for(y in 1970:2000){
  tmean_y <- sapply(gcms, function(m){
    load(file.path("/home/victor/projects/future_forests/data/climate", ssp, m, paste0("bioclim_format/biovars_",y,".Rdata")))
    biovars <- crop(rast(biovars[,c('lon', 'lat', 'bio1')]), med, mask = T)
    return(values(biovars, na.rm = T))
  })
  tmean_y <- rowMeans(tmean_y)
  tmean_hist_med <- cbind(tmean_hist_med, tmean_y)
}
tmean_hist_med <- rowMeans(tmean_hist_med)

tmean_20802100_med <- c()
for(y in 2080:2100){
  tmean_y <- sapply(gcms, function(m){
    load(file.path("/home/victor/projects/future_forests/data/climate", ssp, m, paste0("bioclim_format/biovars_",y,".Rdata")))
    biovars <- crop(rast(biovars[,c('lon', 'lat', 'bio1')]), med, mask = T)
    return(values(biovars, na.rm = T))
  })
  tmean_y <- rowMeans(tmean_y)
  tmean_20802100_med <- cbind(tmean_20802100_med, tmean_y)
}
tmean_20802100_med <- rowMeans(tmean_20802100_med)




idxs <- which((grepl(ssp,names(mech)) | grepl('hist',names(mech))) & time(mech, format ="years") %in% 1970:2000)
r <- mean(crop(subset(mech,idxs), med, mask = T))
names(r) <- 'mech_fitness'
mech_fitness_med_hist <- as.data.frame(r)
mech_fitness_med_hist <- (mech_fitness_med_hist$mech_fitness-min(mech_fitness))/(max(mech_fitness)-min(mech_fitness)) # scale to 0 - 1

idxs <- which((grepl(ssp,names(mech)) | grepl('hist',names(mech))) & time(mech, format ="years") %in% 2080:2100)
r <- mean(crop(subset(mech,idxs), med, mask = T))
names(r) <- 'mech_fitness'
mech_fitness_med_20802100 <- as.data.frame(r)
mech_fitness_med_20802100 <- (mech_fitness_med_20802100$mech_fitness-min(mech_fitness))/(max(mech_fitness)-min(mech_fitness)) # scale to 0 - 1

periods <- c('historical')
csdm_fitness_med_hist <- sapply(sdms, function(s){
  fit <- sapply(gcms, function(m){
    fit <- sapply(periods, function(p){
      print(p)
      if(p == 'historical'){
        proj <- readRDS(file.path("/home/victor/projects/future_forests/data/simulations/fagus_sylvatica",s, m, paste0(p, ".rds")))
        proj <- crop(rast(proj[,c(2,1,3)]), med, mask = T)
        return(values(proj, na.rm = T))
      }
      proj <- readRDS(file.path("/home/victor/projects/future_forests/data/simulations/fagus_sylvatica",s, m, "ssp245", paste0(p, ".rds")))
      proj <- crop(rast(proj[,c(2,1,3)]), med, mask = T)
      return(values(proj, na.rm = T))
    })
    fit <- rowMeans(fit)
  })
  fit <- rowMeans(fit)
  return(fit)
})
csdm_fitness_med_hist <- rowMeans(csdm_fitness_med_hist)
csdm_fitness_med_hist <- (csdm_fitness_med_hist-min(csdm_fitness))/(max(csdm_fitness)-min(csdm_fitness)) # scale to 0 - 1

periods <- c('2080_2100')
csdm_fitness_med_20802100 <- sapply(sdms, function(s){
  fit <- sapply(gcms, function(m){
    fit <- sapply(periods, function(p){
      if(p == 'historical'){
        proj <- readRDS(file.path("/home/victor/projects/future_forests/data/simulations/fagus_sylvatica",s, m, paste0(p, ".rds")))
        proj <- crop(rast(proj[,c(2,1,3)]), med, mask = T)
        return(values(proj, na.rm = T))
      }
      proj <- readRDS(file.path("/home/victor/projects/future_forests/data/simulations/fagus_sylvatica",s, m, "ssp245", paste0(p, ".rds")))
      proj <- crop(rast(proj[,c(2,1,3)]), med, mask = T)
      return(values(proj, na.rm = T))
    })
    fit <- rowMeans(fit)
  })
  fit <- rowMeans(fit)
  return(fit)
})
csdm_fitness_med_20802100 <- rowMeans(csdm_fitness_med_20802100)
csdm_fitness_med_20802100 <- (csdm_fitness_med_20802100-min(csdm_fitness))/(max(csdm_fitness)-min(csdm_fitness)) # scale to 0 - 1










tmean_hist_bor <- c()
for(y in 1970:2000){
  tmean_y <- sapply(gcms, function(m){
    load(file.path("/home/victor/projects/future_forests/data/climate", ssp, m, paste0("bioclim_format/biovars_",y,".Rdata")))
    biovars <- crop(rast(biovars[,c('lon', 'lat', 'bio1')]), bor, mask = T)
    return(values(biovars, na.rm = T))
  })
  tmean_y <- rowMeans(tmean_y)
  tmean_hist_bor <- cbind(tmean_hist_bor, tmean_y)
}
tmean_hist_bor <- rowMeans(tmean_hist_bor)

tmean_20802100_bor <- c()
for(y in 2080:2100){
  tmean_y <- sapply(gcms, function(m){
    load(file.path("/home/victor/projects/future_forests/data/climate", ssp, m, paste0("bioclim_format/biovars_",y,".Rdata")))
    biovars <- crop(rast(biovars[,c('lon', 'lat', 'bio1')]), bor, mask = T)
    return(values(biovars, na.rm = T))
  })
  tmean_y <- rowMeans(tmean_y)
  tmean_20802100_bor <- cbind(tmean_20802100_bor, tmean_y)
}
tmean_20802100_bor <- rowMeans(tmean_20802100_bor)




idxs <- which((grepl(ssp,names(mech)) | grepl('hist',names(mech))) & time(mech, format ="years") %in% 1970:2000)
r <- mean(crop(subset(mech,idxs), bor, mask = T))
names(r) <- 'mech_fitness'
mech_fitness_bor_hist <- as.data.frame(r)
mech_fitness_bor_hist <- (mech_fitness_bor_hist$mech_fitness-min(mech_fitness))/(max(mech_fitness)-min(mech_fitness)) # scale to 0 - 1

idxs <- which((grepl(ssp,names(mech)) | grepl('hist',names(mech))) & time(mech, format ="years") %in% 2080:2100)
r <- mean(crop(subset(mech,idxs), bor, mask = T))
names(r) <- 'mech_fitness'
mech_fitness_bor_20802100 <- as.data.frame(r)
mech_fitness_bor_20802100 <- (mech_fitness_bor_20802100$mech_fitness-min(mech_fitness))/(max(mech_fitness)-min(mech_fitness)) # scale to 0 - 1

periods <- c('historical')
csdm_fitness_bor_hist <- sapply(sdms, function(s){
  fit <- sapply(gcms, function(m){
    fit <- sapply(periods, function(p){
      print(p)
      if(p == 'historical'){
        proj <- readRDS(file.path("/home/victor/projects/future_forests/data/simulations/fagus_sylvatica",s, m, paste0(p, ".rds")))
        proj <- crop(rast(proj[,c(2,1,3)]), bor, mask = T)
        return(values(proj, na.rm = T))
      }
      proj <- readRDS(file.path("/home/victor/projects/future_forests/data/simulations/fagus_sylvatica",s, m, "ssp245", paste0(p, ".rds")))
      proj <- crop(rast(proj[,c(2,1,3)]), bor, mask = T)
      return(values(proj, na.rm = T))
    })
    fit <- rowMeans(fit)
  })
  fit <- rowMeans(fit)
  return(fit)
})
csdm_fitness_bor_hist <- rowMeans(csdm_fitness_bor_hist)
csdm_fitness_bor_hist <- (csdm_fitness_bor_hist-min(csdm_fitness))/(max(csdm_fitness)-min(csdm_fitness)) # scale to 0 - 1

periods <- c('2080_2100')
csdm_fitness_bor_20802100 <- sapply(sdms, function(s){
  fit <- sapply(gcms, function(m){
    fit <- sapply(periods, function(p){
      if(p == 'historical'){
        proj <- readRDS(file.path("/home/victor/projects/future_forests/data/simulations/fagus_sylvatica",s, m, paste0(p, ".rds")))
        proj <- crop(rast(proj[,c(2,1,3)]), bor, mask = T)
        return(values(proj, na.rm = T))
      }
      proj <- readRDS(file.path("/home/victor/projects/future_forests/data/simulations/fagus_sylvatica",s, m, "ssp245", paste0(p, ".rds")))
      proj <- crop(rast(proj[,c(2,1,3)]), bor, mask = T)
      return(values(proj, na.rm = T))
    })
    fit <- rowMeans(fit)
  })
  fit <- rowMeans(fit)
  return(fit)
})
csdm_fitness_bor_20802100 <- rowMeans(csdm_fitness_bor_20802100)
csdm_fitness_bor_20802100 <- (csdm_fitness_bor_20802100-min(csdm_fitness))/(max(csdm_fitness)-min(csdm_fitness)) # scale to 0 - 1