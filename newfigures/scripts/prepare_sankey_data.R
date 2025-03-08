
prepare_sankey_data <- function(
    species, type, nsim,
    scenario = "ssp245",
    wd = "~/projects/future_forests"){
  
  ths_ag_dist <- 0.5*nsim
  
  sim_dist_2090 <- readRDS(file = file.path(wd,"newfigures","data","maps", paste0(species, "_", type, "_2090_dist_", scenario, ".rds")))
  
  sim_distribution <- readRDS(file = file.path(wd,"newfigures","data","maps", paste0(species, "_", type, "_distribution_", scenario, ".rds")))
  crs(sim_distribution) <- "EPSG:4326"
  
  histemp <- subset(sim_dist_2090, 2)
  histemp <- ifel(histemp == '-1', 1, 0)
  histemp <- ifel(is.na(histemp), 0, histemp)
  histr <- subset(sim_dist_2090, 1)
  histr <- histr + histemp
  
  dist <- sim_distribution %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) %>% project("EPSG:3035")
  dist <- ifel(dist > ths_ag_dist, 1, 0)
  dist <- c(histr, dist)
  
  sankeyd <- as.data.frame(dist) 
  sankeyd <- sankeyd[which(!(sankeyd$historical == 0 & sankeyd$`2050` == 0 & sankeyd$`2090` == 0)), ]
  # sankeyd <- sankeyd[which(!(sankeyd$historical == 0 & sankeyd$`2050` == 1 & sankeyd$`2090` == 0)), ]
  ind <- which((sankeyd$historical == 0 & sankeyd$`2050` == 0 & sankeyd$`2090` == 1))
  sankeyd[ind, 'historical'] <- 4
  sankeyd[ind, '2050'] <- 3
  sankeyd[ind, '2090'] <- 2
  ind <- which((sankeyd$historical == 0 & sankeyd$`2050` == 1 & sankeyd$`2090` == 1))
  sankeyd[ind, 'historical'] <- 3
  sankeyd[ind, '2050'] <- 2
  sankeyd[ind, '2090'] <- 2
  #sankeyd[which((sankeyd$historical == 2 & sankeyd$`2050` == 2 & sankeyd$`2090` == 1)), '2090'] <- 2
  
  sankeyd$historical <- paste('hist', sankeyd$historical)
  sankeyd$`2050` <- paste('y2050', sankeyd$`2050`)
  sankeyd$`2090` <- paste('y2090', sankeyd$`2090`)
  
  sankeyd <- sankeyd %>%
    make_long(historical, `2050`, `2090`)
  
  return(sankeyd)
  
}




