




#switch
reload_data <- TRUE 
target_ext <- ext(-10.5, 31.7, 34.6, 71.2)
ytext <- 0.4

gcms <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
scenarios <- c("ssp245", "ssp585")
baseline <- 1970:2000 # define temporal baseline (= reference)
years <- c(2050, 2090)

species <- "fraxinus_excelsior"

csdms <- c("lasso_glm", "gam", "brt", "random_forest")
pbms <- list(fraxinus_excelsior = c("expert",
                                 paste0("subset",rep(1:2, each = 5),"_rep", 1:5)))

# load simulations
if(reload_data){
  # load simulations of PBMs
  ncores <- 5
  source(file.path(wd, "newfigures", "scripts", "fig4", "load_yearly_fitness_pbm_all.R"))
  saveRDS(simulations_pbm, file.path(wd,"figures_cleaned","data", "sims", paste0("pbm_", species, "_Europe", ".rds")))
  #switch
  reload_data <- FALSE 
  target_ext <- ext(-10.5, 31.7, 34.6, 71.2)
  ytext <- 0.4
  
  gcms <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
  scenarios <- c("ssp245", "ssp585")
  scenario <- "ssp245"
  baseline <- 1970:2000 # define temporal baseline (= reference)
  years <- c(2050, 2090)
  
  species <- "fagus_sylvatica"
  
  csdms <- c("lasso_glm", "gam", "brt", "random_forest")
  pbms <- list(fagus_sylvatica = c("expert",
                                   paste0("subset",rep(1:10, each = 10),"_rep", 1:10)))
  
  # load simulations
  if(reload_data){
    # load simulations of PBMs
    ncores <- 5
    source(file.path(wd, "newfigures", "scripts", "fig4", "load_yearly_fitness_pbm_all.R"))
    saveRDS(simulations_pbm, file.path(wd,"figures_cleaned","data", "sims", paste0("pbm_", species, "_Europe", ".rds")))
    
    source(file.path(wd, "figures_cleaned", "scripts", "fig4", "load_yearly_fitness_csdm_all.R"))
    saveRDS(simulations_csdm, file.path(wd,"figures_cleaned","data", "sims", paste0("csdm_", species, "_Europe", ".rds")))
    
  }else{
    simulations_pbm <- readRDS(file = file.path(wd,"figures_cleaned","data", "sims", paste0("pbm_", species, "_Europe", ".rds")))
    simulations_csdm <- readRDS(file = file.path(wd,"figures_cleaned","data", "sims", paste0("csdm_", species, "_Europe", ".rds")))
  }
  
  source(file.path(wd, "figures_cleaned", "scripts", "fig4", "load_yearly_fitness_csdm_all.R"))
  saveRDS(simulations_csdm, file.path(wd,"figures_cleaned","data", "sims", paste0("csdm_", species, "_Europe", ".rds")))
  
}else{
  simulations_pbm <- readRDS(file = file.path(wd,"figures_cleaned","data", "sims", paste0("pbm_", species, "_Europe", ".rds")))
  simulations_csdm <- readRDS(file = file.path(wd,"figures_cleaned","data", "sims", paste0("csdm_", species, "_Europe", ".rds")))
}
