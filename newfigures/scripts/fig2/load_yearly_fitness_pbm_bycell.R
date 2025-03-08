
#----------------------------------#
# Load yearly simulations, by cell #
#----------------------------------#

# for 2080-2100

# number of SSPs/GCMs/SDMs
Ngcm <- length(gcms)
Nssp <- length(scenarios)
Nsdm <- length(pbms[[species]])

# degree for polynomial fitting (if needed)
ndeg_poly <- 4

plan(multisession, workers = ncores)
simulations_pbm <- foreach(c = 1:Nsdm, .combine=rbind) %dofuture% {
  
  ci <- pbms[[species]][c]
  
  suit <- rast(file.path(wd, "data", "processed", species, "suit", paste0(ci, ".tif")))
  
  sims <- data.frame()
  for(m in 1:Ngcm){
    #print(m)
    
    mi <- gcms[m]
    indices <- which(names(suit) == paste0(mi, "_hist") & time(suit, format ="years") %in% baseline)
    ref <- mean(subset(suit,indices))
    
    for(s in 1:Nssp){
      
      si <- scenarios[s]
      
      # indices_hist <- which(names(suit) == paste0(mi, "_hist") & time(suit, format ="years") %in% c(1970:2010))
      # indices <- c(indices_hist, which(names(suit) == paste0(mi, "_", si)))
      indices <- which(names(suit) == paste0(mi, "_", si) & time(suit, format ="years") %in% c(2080:2100))
      
      y <- mean(subset(suit,indices))
      
      # x <- lubridate::year(time(y))
      # x <- x - (min(x)) + 1
      
      # absolute change
      y <- (y-ref)
      
      # plot(y~x) ; lines(smy~x)
      
      sim_temp <- as.data.frame(y, xy = TRUE)
      sim_temp$cal <- ci
      sim_temp$gcm <- mi
      sim_temp$ssp <- si
      sim_temp$id_cell <- 1:nrow(sim_temp)
      sim_temp$method <- ifelse(sim_temp$cal == "expert", "expert", "fitted")
      
      sims <- rbind(
        sims,
        sim_temp
      )
      
    }
  }
  sims
}
plan(sequential);gc()
