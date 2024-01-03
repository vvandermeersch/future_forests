



wd <- "E:/USERS/VanderMeersch/projects/future_forests"

source(file.path(wd, "scripts", "simulations", "functions", "run_phenofit.R"))

cmip6_folder <- "D:/CMIP6-Adjust"
gcm <- "IPSL-CM6A-LR"
run_phenofit(species_file = file.path(wd, "data", "models", "phenofit", "expert", "fagus_sylvatica.species"),
             years = c(2017, 2019), 
             output_dir = file.path(wd, "data", "simulations", "ssp585", gcm, "fagus_sylvatica", "phenofit", "expert"), 
             clim_name = gcm, 
             data_dir = file.path(cmip6_folder, gcm, "ssp585", "phenofit_format"),
             quiet_mode = TRUE)

fitness <- read_mean_outputvalue(
  output_folder = file.path(wd, "data", "simulations", "ssp585", gcm, "fagus_sylvatica", "phenofit", "expert"),
  model = "PHENOFIT", output_var = "Fitness")
plot(rast(fitness[,c(2,1,3)]))
