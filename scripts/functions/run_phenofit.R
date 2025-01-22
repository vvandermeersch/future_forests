
# Here's an example of how to run PHENOFIT with this function:
# cmip6_folder <- "D:/CMIP6-Adjust"
# gcm <- "IPSL-CM6A-LR"
# run_phenofit(species_file = file.path(wd, "data", "models", "phenofit", "expert", "fagus_sylvatica.species"),
#              years = c(2020, 2100), 
#              output_dir = file.path(wd, "data", "simulations", "ssp585", gcm, "fagus_sylvatica", "phenofit", "expert"), 
#              clim_name = gcm, 
#              data_dir = file.path(cmip6_folder, gcm, "ssp585", "phenofit_format"),
#              mem = 80000, quiet_mode = TRUE)

run_phenofit <- 
  function(species_file, years,
           output_dir, 
           clim_name, data_dir, 
           cd_capsis = "cd/d E:/USERS/VanderMeersch/applications/capsis4", 
           java8 = NULL, script_name = "ScriptVictor",
           mem = 10000, quiet_mode = TRUE){
    
    command_file <- file.path(tempdir(), "command_file.txt") # I don't care about the commandfile so I just put it in the (per-session) temporary directory
    run_capsis <- paste0("capsis -p script phenofit4.myscripts.", script_name, " ", command_file)
    run <- ifelse(is.null(java8), paste(cd_capsis, paste0("setmem ", mem) , run_capsis, sep=' && '),
                  paste(java8, cd_capsis, paste0("setmem ", mem) , run_capsis, sep=' && '))
    .command_file_setup(command_file = file.path(tempdir(), "command_file.txt"),
                        species_file, output_dir, clim_name, data_dir, years , quiet_mode)
    
    cat("Run PHENOFIT...\n")
    cat(paste0("   Writing output to: ", output_dir, "\n"))
    
    start <- Sys.time()
    shell(run, intern = quiet_mode)
    end <- Sys.time()
    runtime <- end - start
    cat(paste0("   Runtime: ", round(as.numeric(runtime, units = "mins"),1), "min\n"))
    
  }


.command_file_setup <- 
  function(command_file, species_files, output_dir, clim_name, data_dir, years, quiet_mode){
    
    command_lines <- c()
    command_lines[1] <- c("# File for Phenofit4 CommandScript")
    command_lines[2] <- c(paste0("# Generated with Rstudio (time: ", Sys.time(),", user: ",Sys.info()[["user"]],")"))
    command_lines[3] <- c("\t\t\t\t\t")
    command_lines[4] <- c(paste("speciesInputDir ="))
    command_lines[5] <- c(paste("climateInputDir ="))
    command_lines[6] <- c(paste("outputDir =", output_dir))
    command_lines[7] <- c(paste("quietMode =", ifelse(quiet_mode, "true", "false")))
    command_lines[8] <- c("\t\t\t\t\t")
    command_lines[9] <- c("# speciesFileName\tclimateFolderName\tclimateScenario\tstartingYear\tendingYear")
    for(i in 1:length(species_files)){
      command_lines[9+i] <- paste(species_files[i], data_dir, 
                                  clim_name, years[1], years[2], sep="\t")
    }
    
    writeLines(command_lines, command_file)
    
  }
