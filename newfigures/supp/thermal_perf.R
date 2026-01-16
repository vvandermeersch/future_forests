
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

fit_df$tmean_approx <- ceiling(fit_df$tmean / 0.5) * 0.5

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

mech_fitness_agg <- aggregate(mech_fitness ~ tmean_approx, data = fit_df, FUN = quantile, probs = c(0.05, 0.5, 0.95))
csdm_fitness_agg <- aggregate(csdm_fitness ~ tmean_approx, data = fit_df, FUN = quantile, probs = c(0.05, 0.5, 0.95))
fitness_agg <- merge(mech_fitness_agg, csdm_fitness_agg)

# ggplot() +
#   geom_density(aes(x = tmean, weight = mech_fitness), data = fit_df) +
#   geom_density(aes(x = tmean, weight = csdm_fitness), data = fit_df, linetype = 'dashed') +
#   theme_classic()

ggplot(data = fitness_agg) +
  geom_ribbon(aes(x = tmean_approx, ymin = mech_fitness[,'5%'], ymax = mech_fitness[,'95%']),
              alpha = 0.3, fill = '#DCB0F2') +
  geom_line(aes(x = tmean_approx, y = mech_fitness[,'5%']), linetype = 'dashed', color = '#DCB0F2') +
  geom_line(aes(x = tmean_approx, y = mech_fitness[,'50%']), linetype = 'solid', color = '#DCB0F2') +
  geom_line(aes(x = tmean_approx, y = mech_fitness[,'95%']), linetype = 'dashed', color = '#DCB0F2') +
  geom_ribbon(aes(x = tmean_approx, ymin = -csdm_fitness[,'5%'], ymax = -csdm_fitness[,'95%']),
              alpha = 0.3, fill = '#9EB9F3') +
  geom_line(aes(x = tmean_approx, y = -csdm_fitness[,'5%']), linetype = 'dashed', color = '#9EB9F3') +
  geom_line(aes(x = tmean_approx, y = -csdm_fitness[,'50%']), linetype = 'solid', color = '#9EB9F3') +
  geom_line(aes(x = tmean_approx, y = -csdm_fitness[,'95%']), linetype = 'dashed', color = '#9EB9F3') +
  # geom_point(x = mean(tmean_2020.2040), y = mean(mech_fitness_2020.2040$mech_fitness), size = 1) +
  # geom_point(x = mean(tmean_2080.2100), y = mean(mech_fitness_2080.2100$mech_fitness), size = 1) +
  theme_void()

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


idxs <- which(grepl(ssp,names(mech)) & time(mech, format ="years") %in% 2080:2100)
r <- mean(subset(mech,idxs))
names(r) <- 'mech_fitness'
mech_fitness_2080.2100 <- as.data.frame(r)

tmean_2080.2100 <- c()
for(y in 2080:2100){
  tmean_y <- sapply(gcms, function(m){
    load(file.path("/home/victor/projects/future_forests/data/climate", ssp, m, paste0("bioclim_format/biovars_",y,".Rdata")))
    return(biovars$bio1)
  })
  tmean_y <- rowMeans(tmean_y)
  tmean_2080.2100 <- cbind(tmean_2080.2100, tmean_y)
}
tmean_2080.2100  <- rowMeans(tmean_2080.2100)
