

dir <- "C:/Users/vandermeersch/Documents/CEFE/projects/future_forests"
data_dir <- "D:/projects/future_forests"

gcms <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
scenarios <- c("ssp245", "ssp585")

species <- "fagus_sylvatica"

years <- c(1970:2100)
calibrations <- (paste0("subset",rep(7:10, each = 10),"_rep", 1:10))


# save rasters of simulations
plan(multisession, workers = 10)
simulations <- future_lapply(calibrations, function(c){
  # loop on GCMs
  output <- rast(lapply(gcms, function(m){
    # loop on years
    output <- rast(lapply(years, function(yr){
      if(yr <= 2000){
        sim_dir <- file.path(data_dir, "data", "simulations", species, m, "1970_2000", c)
        output <- read_mean_outputvalue(sim_dir, years = yr, 
                                        model = "PHENOFIT", output_var = "Fitness")
        output <- rast(output[,c(2,1,3)])
        time(output) <- as.Date(paste0(yr, "-01-01"))
        names(output) <- paste0(m, "_hist")
      }else if(yr <= 2010){
        sim_dir <- file.path(data_dir, "data", "simulations", species, m, "2001_2010", c)
        output <- read_mean_outputvalue(sim_dir, years = yr, 
                                        model = "PHENOFIT", output_var = "Fitness")
        output <- rast(output[,c(2,1,3)])
        time(output) <- as.Date(paste0(yr, "-01-01"))
        names(output) <- paste0(m, "_hist")
      }else if(yr <= 2019){
        # loop on scenarios
        output <- rast(lapply(scenarios, function(s){
          sim_dir <- file.path(data_dir, "data", "simulations", species, m, s, "2011_2019", c)
          output <- read_mean_outputvalue(sim_dir, years = yr, 
                                          model = "PHENOFIT", output_var = "Fitness")
          output <- rast(output[,c(2,1,3)])
          time(output) <- as.Date(paste0(yr, "-01-01"))
          names(output) <- paste0(m, "_", s)
          return(output)
        }))
      }else if(yr <= 2100){
        # loop on scenarios
        output <- rast(lapply(scenarios, function(s){
          sim_dir <- file.path(data_dir, "data", "simulations", species, m, s, "2020_2100", c)
          output <- read_mean_outputvalue(sim_dir, years = yr, 
                                          model = "PHENOFIT", output_var = "Fitness")
          output <- rast(output[,c(2,1,3)])
          time(output) <- as.Date(paste0(yr, "-01-01"))
          names(output) <- paste0(m, "_", s)
          return(output)
        }))
      }else{stop("Wrong year?")}
      return(output)
    }))
    return(output)
  }))
  
  terra::writeRaster(output, file.path(dir, "process", species, "suit", paste0(c, ".tif")),  overwrite=TRUE)
  
  # .output <- wrap(output)
  # return(.output)
  return(c)
})
plan(sequential);gc()
# names(simulations) <- calibrations
# 
# plot(rast(simulations$subset1_rep1))
# 
# 
# plot(rast(simulations["subset2_rep1"]))
# yr <- 2020
# ssp <- "ssp585"
# indices <- which(time(output, format ="years") == yr & names(output) == "IPSL-CM6A-LR_ssp585")



