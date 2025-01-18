#----------------------------------------------#
# Small script to calculate best threshold/GCM #
#----------------------------------------------#

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/future_forests"
library(data.table)
library(dplyr)
library(AUC)
source(file.path(wd, "scripts", "functions", "compute_best_threshold.R"))
source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))

calibrations <- c(paste0("subset",rep(2, each = 5),"_rep", 1:5))
# calibrations <- c(paste0("partial/drought_frost/subset",rep(1, each = 1),"_rep", 1:5))

source <- c("GFDL-ESM4")
# source <- "ERA5-LAND"
sim_dir <- file.path("D:/projects/future_forests", "data", "simulations")
#sim_dir <- file.path(wd, "data", "simulations")

output_dir <- file.path(wd, "data", "fit")

# species data
sp_folder <- "D:/species/processed"
sp_presabs <- readRDS(file.path(sp_folder, "quercus_pubescens/quercus_pubescens_presabs.rds"))
sp_name <- "quercus_pubescens"


for(m in source){
  dir.create(file.path(output_dir, m, sp_name))
  for(c in calibrations){
    
    # simpath <- file.path(sim_dir, "historical", m, sp_name, c)
    simpath <- file.path(sim_dir, sp_name, m, "1970_2000", c)
    
    output <- read_mean_outputvalue(simpath, 
                                    years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")
    names(output) <- c("lat", "lon", "pred")
    compute_best_threshold(output, sp_presabs, sp_name, filename = c, dir = file.path(output_dir, m))
    
  }
}

