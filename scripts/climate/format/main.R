
#----------------------------#
# Format future climate data #
#----------------------------#

library(terra)
terraOptions(memfrac=0.9)
library(data.table)

cmip6_dir <- "D:/CMIP6-Adjust"
scenario <- "ssp585"

locs <- fread("E:/USERS/VanderMeersch/data/climate/ERA5_Land/phenofit_format/ERA5LAND_Altitude.fit")
names(locs) <- c("lat", "lon", "alt")

# IPSL model (gregorian calendar)
model <- "IPSL-CM6A-LR"
format_phenofit(years = c(2016,2025), extent = ext(c(-14.05, 40.05, 34.65, 71.15)), model, scenario, 
                locs = locs, folder = cmip6_dir, correct_days = TRUE)

# GFDL model (365-day calendar)
model <- "GFDL-ESM4" 
format_phenofit(years = c(1976,2100), extent = ext(c(-13,40,33,72)), model, scenario, folder = cmip6_dir, correct_days = FALSE)

# MPI model (gregorian calendar)
model <- "MPI-ESM1-2-HR"
format_phenofit(years = c(1976,1999), extent = ext(c(-13,40,33,72)), model, scenario, folder = cmip6_dir, correct_days = FALSE)

# MRI model (gregorian calendar)
model <- "MRI-ESM2-0"
format_phenofit(years = c(2000,2100), extent = ext(c(-13,40,33,72)), model, scenario, folder = cmip6_dir, correct_days = FALSE)

# UKESM model (360-day calendar)
model <- "UKESM1-0-LL"
format_phenofit(years = c(2000,2100), extent = ext(c(-13,40,33,72)), model, scenario, variant = "r1i1p1f2",
                folder = cmip6_dir, correct_days = FALSE)


