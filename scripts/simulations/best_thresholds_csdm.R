#--------------------------------------------------------#
# Small script to calculate best threshold/GCM, for CSDM #
#--------------------------------------------------------#

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/future_forests"
library(data.table)
library(dplyr)
library(AUC)
source(file.path(wd, "scripts", "functions", "compute_best_threshold.R"))

models <- "lasso_glm"

source <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")

output_dir <- file.path(wd, "data", "fit")

# species data
sp_folder <- "D:/species/processed"
sp_presabs <- readRDS(file.path(sp_folder, "fagus_sylvatica/fagus_sylvatica_presabs.rds"))
sp_name <- "fagus_sylvatica"


for(m in source){
  dir.create(file.path(output_dir, m, sp_name))
  sim_dir <- file.path(wd, "data", "simulations", species, c, m)
  for(c in models){
    
    output <- readRDS(file.path(sim_dir, paste0("historical", ".rds")))
    names(output) <- c("lat", "lon", "pred")
    compute_best_threshold(output, sp_presabs, sp_name, filename = c, dir = file.path(output_dir, m))
    
  }
}

