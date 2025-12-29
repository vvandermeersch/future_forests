
library(terra)

ssp <- 'ssp245'
gcms <- c('MRI-ESM2-0', 'UKESM1-0-LL', 'MPI-ESM1-2-HR', 'IPSL-CM6A-LR', 'GFDL-ESM4')

tmean <- c()
for(y in 2020:2100){
  tmean_y <- sapply(gcms, function(m){
    load(file.path("/home/victor/projects/future_forests/data/climate", ssp, m, paste0("bioclim_format/biovars_",y,".Rdata")))
    return(biovars$bio1)
  })
  tmean_y <- rowMeans(tmean_y)
  tmean <- cbind(tmean, tmean_y)
}
tmean <- rowMeans(tmean)
  
mech <- rast('/home/victor/projects/future_forests/data/processed/fagus_sylvatica/suit/expert.tif')
idxs <- which(grepl(ssp,names(mech)) & time(mech, format ="years") %in% 2020:2100)
r <- mean(subset(mech,idxs))
names(r) <- 'mech_fitness'
mech_fitness <- as.data.frame(r)

sdms <- c('lasso_glm', 'brt', 'gam', 'random_forest')
periods <- c('2020_2040', '2040_2060', '2060_2080', '2080_2100')
csdm_fitness <- sapply(sdms, function(s){
  fit <- sapply(gcms, function(m){
    fit <- sapply(periods, function(p){
      proj <- readRDS(file.path("/home/victor/projects/future_forests/data/simulations/fagus_sylvatica",s, m, "ssp245", paste0(p, ".rds")))
      return(proj$value)
    })
    fit <- rowMeans(fit)
  })
  fit <- rowMeans(fit)
  return(fit)
})
csdm_fitness <- rowMeans(csdm_fitness)

fit_df <- cbind(tmean, mech_fitness, csdm_fitness)

fit_df$tmean_approx <- round(fit_df$tmean, 0)

mech_fitness_agg <- aggregate(mech_fitness ~ tmean_approx, data = fit_df, FUN = mean)
csdm_fitness_agg <- aggregate(csdm_fitness ~ tmean_approx, data = fit_df, FUN = mean)
fitness_agg <- merge(mech_fitness_agg, csdm_fitness_agg)

# ggplot() +
#   geom_density(aes(x = tmean, weight = mech_fitness), data = fit_df) +
#   geom_density(aes(x = tmean, weight = csdm_fitness), data = fit_df, linetype = 'dashed') +
#   theme_classic()

ggplot(data = fitness_agg) +
  geom_line(aes(x = tmean_approx, y = mech_fitness)) +
  geom_line(aes(x = tmean_approx, y = csdm_fitness), linetype = 'dashed') +
  geom_point(x = mean(tmean_2020.2040), y = mean(mech_fitness_2020.2040$mech_fitness), size = 4)


idxs <- which(grepl(ssp,names(mech)) & time(mech, format ="years") %in% 2020:2040)
r <- mean(subset(mech,idxs))
names(r) <- 'mech_fitness'
mech_fitness_2020.2040 <- as.data.frame(r)

tmean_2020.2040 <- c()
for(y in 2020:2040){
  tmean_y <- sapply(gcms, function(m){
    load(file.path("/home/victor/projects/future_forests/data/climate", ssp, m, paste0("bioclim_format/biovars_",y,".Rdata")))
    return(biovars$bio1)
  })
  tmean_y <- rowMeans(tmean_y)
  tmean_2020.2040 <- cbind(tmean_2020.2040, tmean_y)
}
tmean_2020.2040 <- rowMeans(tmean_2020.2040)
