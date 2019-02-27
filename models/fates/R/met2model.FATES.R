#-------------------------------------------------------------------------------
# Copyright (c) 2016 NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

# R Code to convert NetCDF CF met files into NetCDF FATES met files.

##' met2model wrapper for FATES
##' 
##' @title met2model for FATES
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param lst timezone offset to GMT in hours
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbosefor(year in start_year:end_year)
##' @importFrom ncdf4 ncvar_get ncdim_def ncatt_get ncvar_put
met2model.FATES <- function(in.path, in.prefix, outfolder, start_date, end_date, lst = 0, lat, lon, 
                            overwrite = FALSE, verbose = FALSE, leapyear = FALSE) {
  
  # General Structure- FATES Uses Netcdf so we need to rename vars, split files from years into months, and generate the header file
  # Get Met file from inpath.
  # Loop over years (Open nc.file,rename vars,change dimensions as needed,close/save .nc file)
  # close
  # defining temporal dimension needs to be figured out. If we configure FATES to use same tstep then we may not need to change dimensions  
  
  # "EDGEW"    "EDGEE"    "EDGES"   "EDGEN"
  # Defines the gridcell in which you're working
  # Not sure what the difference is between lat and lon
  
  library(PEcAn.utils)
  
  insert <- function(ncout, name, unit, data) {
    var   <- ncdf4::ncvar_def(name = name, units = unit, dim = dim, missval = -6999, verbose = verbose)
    ncout <- ncdf4::ncvar_add(nc = ncout, v = var, verbose = verbose)
    ncvar_put(nc = ncout, varid = name, vals = data)
    return(invisible(ncout))
  }
  
  ## Create output directory
  dir.create(outfolder)
  
  # Process start and end dates
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date   <- as.POSIXlt(end_date, tz = "UTC")
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)
  
  ## Build met
  for (year in start_year:end_year) {
    
    in.file <- file.path(in.path, paste(in.prefix, year, "nc", sep = "."))
    
    if (file.exists(in.file)) {
      
      ## Open netcdf file
      nc <- ncdf4::nc_open(in.file)
      
      ## Extract variables 
      ## These need to be read in and converted to CLM names (all units are correct)
      
      time      <- ncvar_get(nc, "time")
      lat  <- ncvar_get(nc, "latitude")
      lon <- ncvar_get(nc, "longitude")
      
      # FSDS required
      FSDS <- ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")  ## W/m2
      # FLDS not required: calculates based on Temperature, Pressure and Humidity
      FLDS <- try(ncvar_get(nc, "surface_downwelling_longwave_flux_in_air"), silent = TRUE)  ## W/m2
      useFLDS <- is.numeric(FLDS)
      
      # PRECTmms required
      PRECTmms <- ncvar_get(nc, "precipitation_flux")  ## kg/m2/s -> mm/s (same val, diff name)
      # PSRF not required
      PSRF <- try(ncvar_get(nc, "air_pressure"), silent = TRUE)  ## Pa
      usePSRF <- is.numeric(PSRF)
      
      # RH is optional but can stand in for SHUM and TDEW
      RH <- try(ncdf4::ncvar_get(nc, "relative_humidity"), silent = TRUE)
      SHUM <- try(ncdf4::ncvar_get(nc, "specific_humidity"), silent = TRUE)  ## g/g -> kg/kg
      TDEW <- try(ncdf4::ncvar_get(nc, "dew_point_temperature"), silent = TRUE)
      
      useRH <- is.numeric(RH)
      useSHUM <- is.numeric(SHUM)
      useTDEW <- is.numeric(TDEW)
      
      if(!useRH & (!useSHUM | !useTDEW)){
        PEcAn.logger::logger.error("Need at least RH or SHUM and TDEW")
      }
      
      # TBOT required
      TBOT <- ncvar_get(nc, "air_temperature")  ## K
      
      # WIND required
      WIND <- sqrt(ncvar_get(nc, "eastward_wind") ^ 2 + ncvar_get(nc, "northward_wind") ^ 2)  ## m/s
      
      ## Define new variables that are also required for the CLM met drivers
      
      # EDGEW, EDGEE, EDGES, EDGEN
      # Creates edges from atmospheric data
      # we are doing site level runs and thus are making the bounded area 
      # arbitrarily small (offset = 0.1)
      
      offset <- 0.1
      
      EDGEW <- lon
      EDGEE <- lon + offset
      EDGES <- lat 
      EDGEN <- lat + offset
      
      # ZBOT not required and hardcoaded when used. 
      # There's no clear CF convention for this variable 
      # so we can't expect to see it in a PEcAn CF met file
      ZBOT <- 30
      
      # CLM can do both leap year and no leap year. 
      sm <- ifelse(leapyear & lubridate::leap_year(year),
                   c(0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366) * 86400 ,
                   c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365) * 86400 
      )
      
      ## CREATE MONTHLY FILES
      for (mo in 1:12) {
        tsel <- which(time > sm[mo] & time <= sm[mo + 1])
        outfile <- file.path(outfolder, paste0(formatC(year, width = 4, flag = "0"), "-", 
                                               formatC(mo, width = 2, flag = "0"), ".nc"))
        if (file.exists(outfile) & overwrite == FALSE) {
          next
        }
        
        lat.dim  <- ncdim_def(name = "lat", units = "", vals = 1:1, create_dimvar = FALSE)
        lon.dim  <- ncdim_def(name = "lon", units = "", vals = 1:1, create_dimvar = FALSE)
        time.dim <- ncdim_def(name = "time", units = "seconds", vals = time, 
                              create_dimvar = TRUE) # , unlim = TRUE) # Betsy removed unlim = TRUE because example file was not unlimited and unlimited causes problems
        ## docs say this should be time,lat,lon but get error writing unlimited first
        ## http://www.cesm.ucar.edu/models/cesm1.2/clm/models/lnd/clm/doc/UsersGuide/x12979.html
        dim      <- list(time.dim, lat.dim, lon.dim)  
        
        # LATITUDE
        var <- ncdf4::ncvar_def(name = "LATIXY", units = "degree_north", 
                                dim = list(lat.dim, lon.dim), missval = as.numeric(-9999))
        ncout <- ncdf4::nc_create(outfile, vars = var, verbose = verbose)
        ncvar_put(nc = ncout, varid = "LATIXY", vals = lat)
        
        # LONGITUDE
        var <- ncdf4::ncvar_def(name = "LONGXY", units = "degree_east", 
                                dim = list(lat.dim, lon.dim), missval = as.numeric(-9999))
        ncout <- ncdf4::ncvar_add(nc = ncout, v = var, verbose = verbose)
        ncvar_put(nc = ncout, varid = "LONGXY", vals = lon)
        
        ## surface_downwelling_longwave_flux_in_air
        if(useFLDS){
          ncout <- insert(ncout, "FLDS", "W m-2", FLDS)
        }
        
        ## surface_downwelling_shortwave_flux_in_air
        ncout <- insert(ncout, "FSDS", "W m-2", FSDS)
        
        ## precipitation_flux
        ncout <- insert(ncout, "PRECTmms", "mm/s", PRECTmms)
        
        ## air_pressure
        if(usePSRF){
          ncout <- insert(ncout, "PSRF", "Pa", PSRF)
        }
        
        ## relative_humidity
        if(useRH){
          ncout <- insert(ncout, "RH", "%", RH)
        }
        
        ## specific_humidity
        if(useSHUM){
          ncout <- insert(ncout, "SHUM", "kg/kg", SHUM)
        }
        
        if(useTDEW){ 
          ncout <- insert(ncout, "TDEW", "kg/kg", TDEW)
        }
        
        ## air_temperature
        ncout <- insert(ncout, "TBOT", "K", TBOT)
        
        ## eastward_wind & northward_wind
        ncout <- insert(ncout, "WIND", "m/s", WIND)
        
        ncdf4::nc_close(ncout)
        
      }
      
      ncdf4::nc_close(nc)
      
    }  ## end file exists
  }  ### end loop over met files
  
  PEcAn.logger::logger.info("Done with met2model.FATES")
  
  return(data.frame(file = paste0(outfolder, "/"), 
                    host = c(PEcAn.remote::fqdn()), 
                    mimetype = c("application/x-netcdf"), 
                    formatname = c("CLM met"), 
                    startdate = c(start_date), 
                    enddate = c(end_date), 
                    dbfile.name = "", 
                    stringsAsFactors = FALSE))
} # met2model.FATES


