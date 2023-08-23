#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#### SCRIPT INTRODUCTION ####
#
#' @name functions_data.R  
#' @description R script containing all functions relative to data
#               importation and formatting
#' @author Julien Barrere
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#' Function to get the path of a file, and create directories if they don't exist
#' @param file.in character: path of the file, filename included (ex: "plot/plot.png")
create_dir_if_needed <- function(file.in){
  
  path.in <- strsplit(file.in, "/")[[1]]
  if(length(path.in) > 1){
    for(i in 1:(length(path.in)-1)){
      if(i == 1) path.in_i <- path.in[i]
      else path.in_i <- paste(path.in_i, path.in[i], sep = "/")
      if(!dir.exists(path.in_i)) dir.create(path.in_i)
    }
  }
}






#' Prepare a dataframe with the data to download in CHELSA + metadata
#' @param var.in Name of the climatic variable to download (tas, pet, tas_max, tas_min)
#' @param month.in Name of the month for which to download the variable
#' @param year.in Name of the year for which to download variable
#' @param dir.out Directory where to save the data
to_download_CHELSA <- function(var.in, month.in, year.in, dir.out){
  
  # Build the list of all variables to download
  # -- Link to chelsa V2.1 monthly timeseries data
  link.in = "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/"
  # -- Build dataframe
  data.dl.chelsa = expand.grid(var = var.in, month = month.in, year = year.in) %>%
    mutate(var_name = ifelse(var != "pet", as.character(var), "pet_penman")) %>%
    mutate(filename = paste0("CHELSA_", var_name, "_", month, "_", year, "_V.2.1.tif"), 
           url = paste0(link.in, var, "/", filename), 
           filename.out = paste0(dir.out, "/", filename))
  
  
  # Return the files saved
  return(data.dl.chelsa)
}

#' Prepare a dataframe with the data to download in CHELSA + metadata
#' @param data.dl.chelsa dataframe containing info on the chelsa files downloaded
download_CHELSA <- function(data.dl.chelsa){
  
  # Create output directry if needed
  create_dir_if_needed(data.dl.chelsa$filename.out[1])
  
  # Download all files
  for(i in 1:dim(data.dl.chelsa)[1]) download.file(
    data.dl.chelsa$url[i], data.dl.chelsa$filename.out[i], mode="wb")
  
  # Return the files saved
  return(data.dl.chelsa$filename.out)
}


#' Function to extract climatic data for coordinates
#' @param data.dl.chelsa dataframe containing info on the chelsa files downloaded
#' @param chelsa_files character with the name of the raster files saved
#' @param coord coordinates dataset; must have col longitude and latitude, in 4326
extract_chelsa = function(data.dl.chelsa, chelsa_files, coord){
  
  # Initialize output dataset
  data.out = coord
  
  # Loop on all files to extract
  for(i in 1:length(chelsa_files)){
    
    # Printer
    print(paste0("----> extracting file", i, "/", length(chelsa_files)))
    
    # Load raster i
    rast.i = terra::rast(chelsa_files[i])
    
    # Extract data for coordinates
    data.out$value = as.numeric(terra::extract(
      rast.i, cbind(data.out$longitude, data.out$latitude))[, 1])
    
    # Rename the column
    colnames(data.out)[dim(data.out)[2]] = paste0(
      data.dl.chelsa$var[i], ".", data.dl.chelsa$month[i], "_", 
      data.dl.chelsa$year[i])
  }
  
  # Return output 
  return(data.out)
}



#' Function to format for plotting the climatic data extracted
#' @param data.clim climatic data extracted for each spatial point
format.data.chelsa = function(data.clim){
  
  data.clim %>%
    gather(key = "var", value = "value", colnames(.)[grep("_", colnames(.))]) %>%
    mutate(varclim = gsub("\\..+", "", var), 
           month = as.numeric(gsub(".+\\.", "", gsub("\\_.+", "", var))), 
           year = as.numeric(gsub(".+\\_", "", var))) %>%
    dplyr::select(-var) %>%
    spread(key = "varclim", value = "value") %>%
    mutate(tmean = tas/10 - 273.15)
}


#' Function to extract isimip daily data for each point in coord
#' @param coord coordinates data
#' @param isimip_files Files containing isimip climatic data
extract_isimip = function(coord, isimip_files){
  
  # Vector of variables to extract
  var.vec = c("hurs", "pr", "rlds", "rsds", "sfcwind", "tas")
  
  # Sequence of days during the time period
  date_seq = as.character(seq(as.Date("2011-01-01"), as.Date("2014-12-31"), by="days"))
  
  # Loop on all variables to extract
  for(i in 1:length(var.vec)){
    
    # Printer
    print(paste0("Extracting ISIMIP data for ", var.vec[i]))
    
    # File to extract
    file.i = isimip_files[grep(var.vec[i], isimip_files)]
    
    # Raster for variable i
    rast.i = terra::rast(file.i)
    
    # Initialize data for variable i
    coord.i = coord
    
    # Loop on all dates for which to extract data
    for(j in 1:length(date_seq)){
      
      # Extract raster information
      coord.i$value.j = as.numeric(terra::extract(
        rast.i[[j]], cbind(coord$longitude, coord$latitude))[, 1])
      # Change column name
      colnames(coord.i)[dim(coord.i)[2]] = date_seq[j]
    }
    
    # Finish formatting
    coord.i = coord.i %>%
      gather(key = "date", value = "value", date_seq) %>%
      mutate(year = substr(date, 1, 4), 
             month = substr(date, 6, 7), 
             day = substr(date, 9, 10)) %>%
      dplyr::select(colnames(coord), "year", "month", "day", "value") 
    
    # Keep the structure to initialize output dataset
    if(i == 1) out = coord.i %>% dplyr::select(-value)
    
    # Add to the final dataset
    eval(parse(text = paste0("out$", var.vec[i], " = coord.i$value")))
  }
  
  # Return final output
  return(out)
  
}


#' Function to extract elevation from a df with longitude and latitude columns
#' @param coord data with coordinates
add_elevation_to_coord = function(coord){
  
  st_as_sf(coord, coords = c("longitude", "latitude"), crs = 4326, 
           remove = FALSE) %>%
    mutate(elevation = (get_elev_point(., prj = st_crs(.), src = "aws") %>%
                          st_drop_geometry())$elevation) %>%
    st_drop_geometry()
  
}


#' Function to extract climatic data from wordclim for coordinates
#' @param coord coordinates dataset; must have col longitude and latitude, in 4326
#' @param dir.wc directory where worldclim data are stored
extract_wordlclim = function(coord, dir.wc){
  
  # Initialize output dataset
  data.out = coord %>%
    mutate(ID = paste(cs, ID.point, sep = "_"))
  
  # Loop on all files to extract
  for(i in 1:length(list.files(dir.wc))){
    
    # Initialize the data for variable i
    data.i = data.out %>% dplyr::select(ID)
    
    # Identify the name of variable i 
    var.i = gsub(".+\\_", "", list.files(dir.wc)[i])
    
    # List all files to extract for variable i
    files.i = list.files(paste0(dir.wc, "/wc2.1_2.5m_", var.i), full.names = TRUE)
    files.i = files.i[grep("tif", files.i)]
    
    # Loop on all files
    for(j in 1:length(files.i)){
      
      # Identify month j
      month.j = as.numeric(gsub(".+\\_", "", gsub("\\.tif", "", files.i[j])))
      
      # Load raster i
      rast.ij = terra::rast(files.i[j])
      
      # Extract data for coordinates
      data.i$value = as.numeric(terra::extract(
        rast.ij, cbind(data.out$longitude, data.out$latitude))[, 1])
      
      # Rename the column
      colnames(data.i)[dim(data.i)[2]] = paste0(var.i, "_", month.j)
    }
    # Final formatting of data.i
    data.i = data.i %>%
      gather(key = "month", value = "value", colnames(.)[-1]) %>%
      mutate(month = as.numeric(gsub(".+\\_", "", month))) 
    colnames(data.i)[3] = var.i
    
    # Add to final output
    if(i == 1) out = data.i
    else out = left_join(out, data.i, by = c("ID", "month"))
    
  }
  
  # Join coordinates
  out = data.out %>%
    right_join(out, by = "ID") %>%
    dplyr::select(-ID)
  
  # Return output 
  return(out)
}



#' Function to calculate pet with wordclim data based on Zomer et al. 2022
#' @param data.wc worldclim data
#' @param data.isimip.daily daily data from isimip
get_pet_wc = function(data.wc, data.isimip.daily){
  
  data.wc %>%
    # Add name of months, and number of days per month
    left_join(data.frame(month = c(1:12), month.name = month.abb, ndays = c(
      31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)), by = "month") %>%
    # Add relative humidity from isimip
    left_join((data.isimip.daily %>%
                 filter(year == "2011") %>%
                 mutate(month = as.numeric(month)) %>%
                 group_by(cs, ID.point, month) %>%
                 summarize(RH = mean(hurs, na.rm = TRUE))), 
              by = c("cs", "ID.point", "month")) %>%
    mutate(Rs = srad/1000, 
           Rns = Rs*(1 - 0.23), 
           jd = (month - 1)*30 + ndays/2, 
           esTmax = 0.6108*exp((17.27*tmax)/(tmax+237.3)), 
           esTmin = 0.6108*exp((17.27*tmin)/(tmin+237.3)), 
           es = (esTmax + esTmin)/2,
           ea = (RH/100)*es,
           Ra = 4.921*24*(1 + 0.033*cos(360*jd/365)), 
           Rso = Ra*(0.75 + 0.00002*elevation), 
           Rnl = 4.903*10^(-9)*(((tmax+273.16)^4 + (tmin+273.16)^4)/2)*
             (0.34 - 0.14*sqrt(ea))*(1.35*(Rs/Rso) - 0.35), 
           Rn = Rns - Rnl, 
           Delta = (4098*0.6108*exp((17.27*tavg)/(tavg+237.3)))/((tavg+237.3)^2), 
           Pr = 101.3*((293 - 0.0065*elevation)/293)^5.26, 
           Gamma = (0.001013*Pr)/(0.622*2.45), 
           u2 = wind*log(128)/log(661.3), 
           pet_daily = (0.408*Delta*Rn + Gamma*(900/(tavg + 273.16))*u2*(es - ea))/
             (Delta + Gamma*(1 + 0.34*u2)), 
           pet_monthly = pet_daily*ndays
    )
  
}



#' Calculate future climate for coordinates from worldclim historical data 
#' and ISIMIP future data
#' @param dir.isimip directory where isimip data are stored
#' @param data.wc data extracted from worldclim
#' @param ssp.in ssp scenario for which to extract data
#' @param date.in time period (format "2011-2020") for which to extract data
get_future_clim_isimip = function(dir.isimip, data.wc, ssp.in, date.in){
  
  # Variables needed and climatic scnearios
  var.in = c("hurs", "tas", "tasmin", "tasmax", "pr")
  
  # Get first and last year from the period
  year.start = gsub("\\_.+", "", date.in)
  year.end = gsub(".+\\_", "", date.in)
  
  # Sequence of days during the time period
  date_seq = as.character(seq(as.Date(paste0(year.start, "-01-01")), 
                              as.Date(paste0(year.end, "-12-31")), by="days"))
  date.df = data.frame(ID.day = c(1:length(date_seq)), 
                       date = date_seq) %>%
    mutate(year = as.numeric(as.character(substr(date, 1, 4))), 
           month = as.numeric(as.character(substr(date, 6, 7))), 
           day = as.numeric(as.character(substr(date, 9, 10))))
  
  # All years in the dataset
  years = unique(date.df$year)
  
  # Coordinate file
  coord = data.wc %>%
    dplyr::select(cs, ID.point, longitude, latitude) %>%
    distinct()
  
  
  # Loop on all years of the time period
  for(y in 1:length(years)){
    
    # Printer
    print(paste0("Data extraction for year ", years[y]))
    
    # Subset the date data frame with year y
    date.df.y = subset(date.df, year == years[y])
    
    # Loop on all variables
    for(v in 1:length(var.in)){
      
      # Printer
      print(paste0(" -- variable ", var.in[v]))
      
      # List of files
      file.list = list.files(paste(dir.isimip, var.in[v], ssp.in, sep = "/"), 
                             full.names = TRUE)
      file = file.list[grep(year.start, file.list)]
      
      # Read the raster file
      rast.in = terra::rast(file)
      
      # Loop on all days of the year to extract data
      for(d in 1:dim(date.df.y)[1]){
        
        # Initialize output data
        if(d == 1) data.days.i = coord
        
        # Extract climate data from raster
        data.days.i$value = as.numeric(terra::extract(
          rast.in[[date.df.y$ID.day[d]]], cbind(coord$longitude, coord$latitude))[, 1])
        
        # Change column name
        colnames(data.days.i)[dim(data.days.i)[2]] = date.df.y$date[d]
      }
      
      # Reformat data
      data.days.i = data.days.i %>%
        gather(key = "date", value = "variable", date.df.y$date) %>%
        left_join(date.df.y, by = "date") %>%
        dplyr::select("cs", "ID.point", "longitude", "latitude", 
                      "month", "day", "variable")
      colnames(data.days.i)[dim(data.days.i)[2]] = var.in[v]
      
      # Add to final dataset
      if(v == 1) data.days = data.days.i
      else eval(parse(text = paste0(
        "data.days$", var.in[v], " = data.days.i$", var.in[v])))
      
      
    }
    
    # Final formatting
    out.y = data.days %>%
      # Add data from worldclim
      left_join((data.wc %>% dplyr::select(
        cs, ID.point, month, elevation, srad, wind)), 
        by = c("cs", "ID.point", "month")) %>%
      # add julian days
      left_join(date.df.y %>% 
                  mutate(jd = c(1:dim(date.df.y)[1])) %>%
                  dplyr::select(month, day, jd), 
                by = c("month", "day")) %>%
      # Add spp scenario and year
      mutate(ssp = ssp.in, 
             year = years[y]) %>%
      # convert degrees from kalvin to celsius
      mutate(tavg = tas - 273.15, 
             tmin = tasmin - 273.15, 
             tmax = tasmax - 273.15) %>%
      # calculations for pet
      mutate(Rs = srad/1000, 
             Rns = Rs*(1 - 0.23), 
             esTmax = 0.6108*exp((17.27*tmax)/(tmax+237.3)), 
             esTmin = 0.6108*exp((17.27*tmin)/(tmin+237.3)), 
             es = (esTmax + esTmin)/2,
             ea = (hurs/100)*es,
             Ra = 4.921*24*(1 + 0.033*cos(360*jd/365)), 
             Rso = Ra*(0.75 + 0.00002*elevation), 
             Rnl = 4.903*10^(-9)*(((tmax+273.16)^4 + (tmin+273.16)^4)/2)*
               (0.34 - 0.14*sqrt(ea))*(1.35*(Rs/Rso) - 0.35), 
             Rn = Rns - Rnl, 
             Delta = (4098*0.6108*exp((17.27*tavg)/(tavg+237.3)))/((tavg+237.3)^2), 
             Pr = 101.3*((293 - 0.0065*elevation)/293)^5.26, 
             Gamma = (0.001013*Pr)/(0.622*2.45), 
             u2 = wind*log(128)/log(661.3), 
             pet_daily = (0.408*Delta*Rn + Gamma*(900/(tavg + 273.16))*u2*(es - ea))/
               (Delta + Gamma*(1 + 0.34*u2))) %>%
      # calculation for sgdd
      mutate(T55 = ifelse(tavg < 5.5, 0, tavg - 5.5)) %>%
      # Convert precipitation in mm
      mutate(pr = pr*60*60*24) %>%
      # Annual sum
      group_by(cs, ID.point, longitude, latitude, elevation, ssp, year) %>%
      summarize(tmean = mean(tavg, na.rm = TRUE), 
                prec = sum(pr, na.rm = TRUE), 
                pet = sum(pet_daily, na.rm = TRUE), 
                sgdd = sum(T55, na.rm = TRUE)) %>%
      # calculate water aridity index
      mutate(wai = (prec - pet)/pet)
    
    # Add to final output
    if(y == 1) out = out.y
    else out = rbind(out, out.y)
  }
  
  # Return final output
  return(out)
  
}