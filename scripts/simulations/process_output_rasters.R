
dir <- "C:/Users/vandermeersch/Documents/CEFE/projects/future_forests"
ths_dir <- file.path(dir, "data", "fit")
species <- "fagus_sylvatica"

# Transform simulation outputs to suitability
baseline <- 1970:2000 # or 1970:2010 ?
projections <- 1970:2100
window <- 15

calibrations <- (paste0("subset",rep(1, each = 10),"_rep", 1:10))

gcms <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
scenarios <- c("ssp245", "ssp585")
species <- "fagus_sylvatica"

folder_name <- "bin"

plan(multisession, workers = 10)
suitability <- future_lapply(calibrations, function(c){
  # sim <- rast(simulations[c][[1]])
  sim <- rast(file.path(dir, "process", species, "suit", paste0(c, ".tif")))
  
  # loop on GCMs
  proj <- rast(lapply(gcms, function(m){
    
    # load threshold
    ths <- readRDS(file.path(ths_dir, m, species, paste0(c, ".rds")))$best_threshold
    
    # baseline
    indices <- which(time(sim, format ="years") %in% baseline & names(sim) == paste0(m, "_hist"))
    bas <- mean(subset(sim, indices)) # mean fitness over the period
    bas <- ifel(bas < as.numeric(ths), 0, 1) # apply threshold
    time(bas) <- as.Date(paste0(mean(baseline), "-01-01"))
    names(bas) <- paste0(m, "_ref")
    
    # create temporal windows
    r <- embed(projections, window)[, c(window,1)]
    yr_windows <- split(r, row(r))  
    
    # loop on years
    proj <- rast(lapply(yr_windows, function(yr){
      cat((yr[1]+yr[2])/2)
      # loop on scenarios
      proj <- rast(lapply(scenarios, function(s){
        if(yr[1] < 2015){
          ind1 <- which(time(sim, format ="years") %in% c(yr[1]:yr[2]) & names(sim) == paste0(m, "_hist"))
          ind2 <- which(time(sim, format ="years") %in% c(yr[1]:yr[2]) & names(sim) == paste0(m, "_", s))
          indices <- c(ind1, ind2)
        }else{
          indices <- which(time(sim, format ="years") %in% c(yr[1]:yr[2]) & names(sim) == paste0(m, "_", s))
        }
        if(length(indices) != window){stop("Problem with indices and window!")} # check
        proj <- mean(subset(sim, indices)) # mean fitness over the period
        proj <- ifel(proj < as.numeric(ths), 0, 1) # apply threshold
    
        time(proj) <- as.Date(paste0((yr[1]+yr[2])/2, "-01-01"))
        names(proj) <- paste0(m, "_", s)
        return(proj)
      }))
      return(proj)
    }))
    names(proj) <- rep(paste0(m, "_", scenarios), length(yr_windows))
    
    proj <- c(bas, proj)
    return(proj)
  }))
  
  terra::writeRaster(proj, file.path(dir, "process", species, folder_name, paste0(c, ".tif")),  overwrite=TRUE)
  
  # return(proj)
  return(c)
})
plan(sequential);gc()



