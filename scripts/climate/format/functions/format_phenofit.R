
format_phenofit <- function(years, extent, model, scenario, variant = "r1i1p1f1", 
                            locs, folder, correct_days = TRUE){
  
  lpyears <- c(1904, 1908, 1912, 1916, 1920, 1924, 1928, 1932, 1936, 1940, 1944, 
               1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 
               1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020, 2024, 2028, 2032, 
               2036, 2040, 2044, 2048, 2052, 2056, 2060, 2064, 2068, 2072, 2076, 
               2080, 2084, 2088, 2092, 2096)
  
  cat("Formating data for PHENOFIT\n")
  
  files_to_read <- .find_filenames(from = years[1], years[2])
  
  pre <- rast(lapply(files_to_read, function (f)
    rast(paste0(file.path(folder, model, scenario, "raw"), "/", "prAdjust_day_", 
                model, "_", scenario, "_", variant, "_gr010_TCDF-CDFT23-ERA5Land-1981-2010_", f, ".nc"))))
  tmp <- rast(lapply(files_to_read, function (f)
    rast(paste0(file.path(folder, model, scenario, "raw"), "/", "tasAdjust_day_", 
                model, "_", scenario, "_", variant, "_gr010_TCDF-CDFT23-ERA5Land-1981-2010_", f, ".nc"))))
  tmn <- rast(lapply(files_to_read, function (f)
    rast(paste0(file.path(folder, model, scenario, "raw"), "/", "tasminAdjust_day_", 
                model, "_", scenario, "_", variant,"_gr010_TCDF-CDFT23-ERA5Land-1981-2010_", f, ".nc"))))
  tmx <- rast(lapply(files_to_read, function (f)
    rast(paste0(file.path(folder, model, scenario, "raw"), "/", "tasmaxAdjust_day_", 
                model, "_", scenario, "_", variant,"_gr010_TCDF-CDFT23-ERA5Land-1981-2010_", f, ".nc"))))
  rh <- rast(lapply(files_to_read, function (f)
    rast(paste0(file.path(folder, model, scenario, "raw"), "/", "hursAdjust_day_", 
                model, "_", scenario, "_", variant,"_gr010_TCDF-CDFT23-ERA5Land-1981-2010_", f, ".nc"))))
  wnd <- rast(lapply(files_to_read, function (f)
    rast(paste0(file.path(folder, model, scenario, "raw"), "/", "sfcWindAdjust_day_", 
                model, "_", scenario, "_", variant,"_gr010_TCDF-CDFT23-ERA5Land-1981-2010_", f, ".nc"))))
  glo <- rast(lapply(files_to_read, function (f)
    rast(paste0(file.path(folder, model, scenario, "raw"), "/", "rsdsAdjust_day_", 
                model, "_", scenario, "_", variant,"_gr010_TCDF-CDFT23-ERA5Land-1981-2010_", f, ".nc"))))

  out_folder <- file.path(folder, model, scenario, "phenofit_format")
  dir.create(out_folder, showWarnings = FALSE)
  
  locs_r <- rast(locs[,c(2,1,3)])
  
  for(yr in years[1]:years[2]){
    
    true_leap_year <- yr %in% lpyears # is the year supposed to be a leap year ?
    
    cat(paste0("   Year ", yr, "\n"))
    if(true_leap_year){cat(paste0("   (leap year)\n"))}
    
    precrop_extent <- ext(c(0,360,33,72))
    
    cat("    - processing rasters...\n")
    
    indices <- which(time(tmp, format ="years") == yr)
    pre_yr <- subset(pre, indices)
    tmp_yr <- subset(tmp, indices)
    tmn_yr <- subset(tmn, indices)
    tmx_yr <- subset(tmx, indices)
    rh_yr <- subset(rh, indices)
    wnd_yr <- subset(wnd, indices)
    glo_yr <- subset(glo, indices)
    
    
    # pre-cropping (save computation time)
    pre_yr <- crop(pre_yr, precrop_extent)
    tmp_yr <- crop(tmp_yr, precrop_extent)
    tmn_yr <- crop(tmn_yr, precrop_extent)
    tmx_yr <- crop(tmx_yr, precrop_extent)
    rh_yr <- crop(rh_yr, precrop_extent)
    wnd_yr <- crop(wnd_yr, precrop_extent)
    glo_yr <- crop(glo_yr, precrop_extent)
    
    # rotate data along longitude
    pre_yr <- rotate(pre_yr)
    tmp_yr <- rotate(tmp_yr)
    tmn_yr <- rotate(tmn_yr)
    tmx_yr <- rotate(tmx_yr)
    rh_yr <- rotate(rh_yr)
    wnd_yr <- rotate(wnd_yr)
    glo_yr <- rotate(glo_yr)
    
    # crop around Europe (+mask by Europe)
    pre_yr <- mask(crop(pre_yr, extent), locs_r)
    tmp_yr <- mask(crop(tmp_yr, extent), locs_r)
    tmn_yr <- mask(crop(tmn_yr, extent), locs_r)
    tmx_yr <- mask(crop(tmx_yr, extent), locs_r)
    rh_yr <- mask(crop(rh_yr, extent), locs_r)
    wnd_yr <- mask(crop(wnd_yr, extent), locs_r)
    glo_yr <- mask(crop(glo_yr, extent), locs_r)
    
    # convert units, change decimal precision
    pre_yr <- pre_yr*86400 # in mm
    tmp_yr <- tmp_yr-273.15 # in celsius
    tmn_yr <- tmn_yr-273.15 # in celsius
    tmx_yr <- tmx_yr-273.15 # in celsius
    glo_yr <- glo_yr*24*60*60/1000000 # from W.m-2 to MJ.m-2
    pre_yr <- round(pre_yr,2)
    tmp_yr <- round(tmp_yr,2)
    tmn_yr <- round(tmn_yr,2)
    tmx_yr <- round(tmx_yr,2)
    rh_yr <- round(rh_yr,2)
    wnd_yr <- round(wnd_yr,2)
    glo_yr <- round(glo_yr,2)
    
    # convert to dataframe
    pre_df <- as.data.frame(pre_yr, xy = T)
    pre_df <- round(pre_df[,c(2,1,3:ncol(pre_df))],2)
    tmp_df <- as.data.frame(tmp_yr, xy = T)
    tmp_df <- round(tmp_df[,c(2,1,3:ncol(tmp_df))],2)
    tmn_df <- as.data.frame(tmn_yr, xy = T)
    tmn_df <- round(tmn_df[,c(2,1,3:ncol(tmn_df))],2)
    tmx_df <- as.data.frame(tmx_yr, xy = T)
    tmx_df <- round(tmx_df[,c(2,1,3:ncol(tmx_df))],2)
    rh_df <- as.data.frame(rh_yr, xy = T)
    rh_df <- round(rh_df[,c(2,1,3:ncol(rh_df))],2)
    wnd_df <- as.data.frame(wnd_yr, xy = T)
    wnd_df <- round(wnd_df[,c(2,1,3:ncol(wnd_df))],2)
    glo_df <- as.data.frame(glo_yr, xy = T)
    glo_df <- round(glo_df[,c(2,1,3:ncol(glo_df))],2)
    
    # this option allows to transform 365-day or 360-day data to true length year (i.e. taking into account leap year)
    if(correct_days){
      # check true leap year vs GCM leap year (some GCM have no leap year, e.g. GFDL)
      gcm_leap_year <- nlyr(pre_yr) == 366
      intplt_29feb <- true_leap_year != gcm_leap_year
      
      # transform calendar, see notes in function
      pre_df <- .transform_calendar(pre_df, intplt_29feb, model)
      tmp_df <- .transform_calendar(tmp_df, intplt_29feb, model)
      tmn_df <- .transform_calendar(tmn_df, intplt_29feb, model)
      tmx_df <- .transform_calendar(tmx_df, intplt_29feb, model)
      rh_df <- .transform_calendar(rh_df, intplt_29feb, model)
      wnd_df <- .transform_calendar(wnd_df, intplt_29feb, model)
      glo_df <- .transform_calendar(glo_df, intplt_29feb, model)

      cat("      (number of days: ", ncol(pre_df)-2,")\n")
    }
    
    # compute evapotranspiration
    cat("    - computing evapotranspiration...\n")
    pet_df <- .compute_PET(tmp_df, tmn_df, tmx_df, rh_df, glo_df, wnd_df, locs)
    pet_df <- round(pet_df, 2)
  
    cat("    - writings files...\n")
    
    # create files
    pre_file <- .create_phenofit_file(yr, var = "pre", model, out_folder)
    tmp_file <- .create_phenofit_file(yr, var = "tmp", model, out_folder)
    tmn_file <- .create_phenofit_file(yr, var = "tmn", model, out_folder)
    tmx_file <- .create_phenofit_file(yr, var = "tmx", model, out_folder)
    rh_file <- .create_phenofit_file(yr, var = "RH", model, out_folder)
    wnd_file <- .create_phenofit_file(yr, var = "wnd", model, out_folder)
    glo_file <- .create_phenofit_file(yr, var = "glo", model, out_folder)
    pet_file <- .create_phenofit_file(yr, var = "pet", model, out_folder)
    
    # write data
    fwrite(pre_df, file = pre_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
    fwrite(tmp_df, file = tmp_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
    fwrite(tmn_df, file = tmn_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
    fwrite(tmx_df, file = tmx_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
    fwrite(rh_df, file = rh_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
    fwrite(wnd_df, file = wnd_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
    fwrite(glo_df, file = glo_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
    fwrite(pet_df, file = pet_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
    
  }
  
}



.create_phenofit_file <- function(yr, var, model,  pd_folder){
  
  processed_file <- file.path(pd_folder, paste0(model, "_", var, "_", yr, "_dly.fit"))
  if(var == "Altitude" | var == "WHC"){
    processed_file <- file.path(pd_folder, paste0(model, "_", var, ".fit"))
  }
  
  con <- file(processed_file, open="wt")
  writeLines("Climate datafile for Phenofit model", con)
  writeLines(paste0("Created on RStudio, by user ", Sys.getenv("USERNAME") ,", on the ", Sys.Date()), con)
  comments <- .get_comments(var = var)
  writeLines(comments, con)
  writeLines(" ", con)
  close(con)
  
  return(processed_file)
  
}


# Function to get Phenofit input variable definitions
.get_comments <- function(var){
  if(var=='glo'){
    return("Variable : daily global radiation (MJ/m²)")
  }
  else if(var=='pre'){
    return("Variable : daily precipitation, comprising rain and snow (mm)")
  }
  else if(var=='prs'){
    return("Variable : daily mean surface pressure (kPa)")
  }
  else if(var=='RH'){
    return("Variable : daily mean 2m relative humidity (%) calculated with vapor pressure ratio (Clausius-Clapeyron relation)")
  }
  else if(var=='tmn'){
    return("Variable : daily mimimal 2m temperature (°C)")
  }
  else if(var=='dtm'){
    return("Variable : daily mean 2m dewpoint temperature (°C)")
  }
  else if(var=='tmp'){
    return("Variable : daily mean 2m temperature (°C)")
  }
  else if(var=='tmx'){
    return("Variable : daily maximal 2m temperature (°C)")
  }
  else if(var=='wnd'){
    return("Variable : daily mean 10m wind speed (m/s) - used only to compute evapotranspiration")
  }
  else if(var=='RHmin'){
    return("Variable : daily miminal 2m relative humidity (%) - used only to compute evapotranspiration")
  }
  else if(var=='RHmax'){
    return("Variable : daily maximal 2m relative humidity (%) - used only to compute evapotranspiration")
  }
  else if(var=='Altitude'){
    return("Variable : altitude (m) calculated from geopotential height")
  }
  else if(var=='pet'){
    return("Variable : potential evapotranspiration (mm) calculated with Penman-Monteith formulation (FAO-56 hypothetical short grass method)")
  }
  else if(var=='WHC'){
    return("Variable : water holding capacity (mm)")
  }
  else if(var=='TOA'){
    return("TOA radiation (kJ/m²)")
  }
  else if(var=='cld'){
    return("Cloudiness factor")
  }
  else if(var=="mbar"){
    return("Daytime mean optical air mass")
  }
  else if(var=="wind"){
    return("Wind speed")
  }
  
}

.find_filenames <- function(from, to){
  
  if(to > 2101){stop("Error")}
  
  start_years <- c(seq(1966,2086,10),2101)
  years <- c()
  
  for(i in from:to){
    if(i < 1966 & !(1951 %in% years)){years <- c(1951)}
    else{
      for(j in 1:length(start_years)){
        if(i > start_years[j] & i < start_years[j+1] & !(start_years[j] %in% years)){years <- c(years, start_years[j])}
      }
    }
  }
  
  files <- sapply(years, function(i) ifelse(i == 1951 | i == 2086, paste0(i,"0101-", i+14, "1231"), paste0(i,"0101-", i+9, "1231")))
  
  return(files)
}


# Function to transform 360-day or 365-day calendar to gregorian calendar
.transform_calendar <- function(data, intplt_29feb, model){
  
  
  # interpolate 29 february on leap year, if needed
  # data for day February 29 is created by averaging the data found on February 28 and March 1 
  # (except for UKESM, a 360-day model...)
  if(intplt_29feb & model != "UKESM1-0-LL" ){
    data <- cbind(data[,1:61], rowMeans(data[,61:62]),data[,62:367])
  }
  
  
  # if the model is UKESM
  # converted it to a gregorian calendar by interpolating the boundary condition data to match 
  # Feb. 30th and 29th (except in leap years) were dropped
  # and the 31st of Jan, Mar, May, Jul, Aug, Oct, and Dec were interpolated from the days before 
  # and after (see "Note 3" in https://na-cordex.org/time-ranges-calendars.html)
  if(model == "UKESM1-0-LL"){
    if(intplt_29feb){
      data <- 
        cbind(data[,1:2], # lat lon
              data[,3:32], # 30-day Jan
              rowMeans(data[,32:33]), # interpolated 31st of Jan
              data[,33:61], # 29-day Feb (we drop Feb. 30th, ie index 62)
              data[,63:92], # 30-day Mar
              rowMeans(data[,92:93]), # interpolated 31st of Mar
              data[,93:122], # 30-day Apr
              data[,123:152], # 30-day May
              rowMeans(data[,152:153]), # interpolated 31st of May
              data[,153:182], # 30-day June
              data[,183:212], # 30-day Jul
              rowMeans(data[,212:213]), # interpolated 31st of Jul
              data[,213:242], # 30-day Aug
              rowMeans(data[,242:243]), # interpolated 31st of Aug
              data[,243:272], # 30-day Sep
              data[,273:302], # 30-day Oct
              rowMeans(data[,302:303]), # interpolated 31st of Oct
              data[,303:332], # 30-day Nov
              data[,333:362], # 30-day Dec
              rowMeans(data[,c(362,3)]) # interpolated 31st of Dec
        )		
    }
    else{
      data <- 
        cbind(data[,1:2], # lat lon
              data[,3:32], # 30-day Jan
              rowMeans(data[,32:33]), # interpolated 31st of Jan
              data[,33:60], # 28-day Feb (we drop Feb. 29th and 30th, ie index 61 and 62)
              data[,63:92], # 30-day Mar
              rowMeans(data[,92:93]), # interpolated 31st of Mar
              data[,93:122], # 30-day Apr
              data[,123:152], # 30-day May
              rowMeans(data[,152:153]), # interpolated 31st of May
              data[,153:182], # 30-day June
              data[,183:212], # 30-day Jul
              rowMeans(data[,212:213]), # interpolated 31st of Jul
              data[,213:242], # 30-day Aug
              rowMeans(data[,242:243]), # interpolated 31st of Aug
              data[,243:272], # 30-day Sep
              data[,273:302], # 30-day Oct
              rowMeans(data[,302:303]), # interpolated 31st of Oct
              data[,303:332], # 30-day Nov
              data[,333:362], # 30-day Dec
              rowMeans(data[,c(362,3)]) # interpolated 31st of Dec
        )
    }
  }
  
  return(data)
}

# function to compute Penman-Monteith evapotranspiration (adapted from Evapotranspiration R package)
.compute_PET <- function(tmp_df, tmn_df, tmx_df, rh_df, glo_df, wnd_df, locs){
  
  # constants
  lambda <- 2.45 # latent heat of evaporisation
  Gsc <- 0.0820 # solar constant
  sigma <-  4.903e-9 # Stefan-Boltzmann constant
  alpha <- 0.23
  alphaPT <- 1.26 # Priestley-Taylor coefficient
  z_wind <- 10 # height of wind instrument in m (= 10 in ERA5-Land data)
  
  tmp <- as.matrix(tmp_df[, -c(1,2)])
  tmn <- as.matrix(tmn_df[, -c(1,2)])
  tmx <- as.matrix(tmx_df[, -c(1,2)])
  rh <- as.matrix(rh_df[, -c(1,2)])
  glo <- as.matrix(glo_df[, -c(1,2)])
  wnd <- as.matrix(wnd_df[, -c(1,2)])
  
  J <- 1:(ncol(tmp_df)-2) # number of days
  lat <- as.numeric(unlist(locs[,"lat"])) * pi/180 # latitude in radians
  
  # mean saturation vapour pressure
  es_tmx <- 0.6108 * exp(17.27 * tmx/(tmx + 237.3))
  es_tmn <- 0.6108 * exp(17.27 * tmn/(tmn + 237.3))
  es <- (es_tmx + es_tmn)/2
  
  # actual vapour pressure (derived from mean relative humidity)
  ea <- rh/100*es
  
  # slope of saturation vapour pressure curve
  delta <- 4098 * (0.6108 * exp((17.27 * tmp)/(tmp + 237.3)))/((tmp + 237.3)^2)
  
  # psychrometric constant 
  P <- 101.3 * ((293 - 0.0065 * as.numeric(unlist(locs[,"alt"])))/293)^5.26
  gamma <- 0.00163 * P/lambda
  
  dr <- 1 + 0.033 * cos(2 * pi/365 * J) # inverse relative distance Earth-Sun
  solar_declination <- 0.409 * sin(2 * pi/365 * J - 1.39)
  
  # sunset hour angle
  aux <- -tan(lat) %*% t(tan(solar_declination))
  aux[aux > 1] <- 1
  aux[aux < -1] <- -1
  ws <- acos(aux) 
  
  # extraterrestrial radiation 
  Ra <- (1440/pi) * Gsc * t(t(t(t(ws * sin(lat)) * sin(solar_declination)) + t(t(sin(ws) * cos(lat)) * cos(solar_declination))) * dr)
  
  # clear-sky radiation
  Rso <- (0.75 + (2 * 10^-5) * as.numeric(unlist(locs[,"alt"]))) * Ra
  
  # net longwave radiation
  Rnl <- sigma * (0.34 - 0.14 * sqrt(ea)) * 
    ((tmx + 273.2)^4 + (tmn + 273.2)^4)/2 * 
    (1.35 * glo/Rso - 0.35)
  
  # net shortwave radiation
  Rns <- (1 - alpha) * glo
  
  # ETP
  Rn <- Rns - Rnl
  wnd_2m <- wnd * 4.87/log(67.8 * z_wind - 5.42) # wind at 2m
  ETP <- (0.408 * delta * Rn + gamma * 900 * wnd_2m * (es - ea)/
            (tmp + 273))/(delta + gamma * (1 + 0.34 * wnd_2m))
  ETP[ETP < 0] <- 0
  ETP[is.na(ETP)] <- 0
  
  ETP <- cbind(locs[, c("lat", "lon")], as.data.table(ETP)) 
  
  return(ETP)
  
}

