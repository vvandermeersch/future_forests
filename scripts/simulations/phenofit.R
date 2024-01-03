



wd <- "E:/USERS/VanderMeersch/projects/future_forests"

source(file.path(wd, "scripts", "simulations", "functions", "command_file_setup.R"))
source(file.path(wd, "scripts", "simulations", "functions", "read_mean_outputvalue.R"))


# Model settings
species_file <- "D:/calibrations/phenofit/quercus_ilex/1000pres_1000abs/paper_data/CMAES/subset_2/cmaes_fit_subset2_rep2.species"
output_folder <- "D:/simulations/phenofit/paleo/fitted/025deg/quercus_ilex_2"
climate_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/dscl_15min"
command_file <- file.path(wd, "phenofit","command_file.txt")

# Capsis settings
capsis_settings=list(java8="java8.cmd", cd_java8="cd C:/Program Files/Java/scripts", cd_capsis="cd/d D:/applications/capsis4 && setmem 10000")
run_capsis <- paste("capsis -p script phenofit4.myscripts.ScriptPaleo", command_file)
run <- paste(capsis_settings$cd_java8, capsis_settings$java8, capsis_settings$cd_capsis, run_capsis, sep=' && ')


# simulation loop
for(year in seq(250, 250, 250)){
  yr_b <- as.character(- year-14)
  yr_e <- as.character(- year+15)
  command_file_setup(command_file, species_file, output_folder, climate_folder,
                     climate_scenario="HadCM3B", starting_year=yr_b, ending_year=yr_e, quiet_mode="true")
  print(year)
  try({system.time(shell(run, intern=FALSE))
    
    fitness <- read_mean_outputvalue(file.path(output_folder, paste0(year, "BP")), output_var = "Fitness")
    names(fitness)[3] <- "pred"
    saveRDS(fitness, file.path(output_folder, paste0(year, "BP.rds")))})
}