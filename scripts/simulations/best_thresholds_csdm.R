#--------------------------------------------------------#
# Small script to calculate best threshold/GCM, for CSDM #
#--------------------------------------------------------#

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/future_forests"
library(data.table)
library(dplyr)
library(AUC)
source(file.path(wd, "scripts", "functions", "compute_best_threshold.R"))

models <- c("lasso_glm", "random_forest", "gam", "brt")

source <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")

output_dir <- file.path(wd, "data", "fit")

# species data
sp_folder <- "D:/species/processed"
sp_presabs <- readRDS(file.path(sp_folder, "fagus_sylvatica/fagus_sylvatica_presabs.rds"))
sp_name <- "fagus_sylvatica"

out_save <- rast()
for(m in source){
  dir.create(file.path(output_dir, m, sp_name))
  
  for(c in models){
    sim_dir <- file.path(wd, "data", "simulations", sp_name, c, m)
    output <- readRDS(file.path(sim_dir, paste0("historical", ".rds")))
    names(output) <- c("lat", "lon", "pred")
    out_r <- rast(output[c(2,1,3)])
    ths <- compute_best_threshold(output, sp_presabs, sp_name, filename = c, dir = file.path(output_dir, m))
    out_r <- ifel(out_r < ths,0,1)
    out_save <- c(out_save, out_r)
  }
}

out_80 <- ifel(sum(out_save) < 0.8*20, 0, 1)
out_50 <- ifel(sum(out_save) < 0.5*20, 0, 1)
