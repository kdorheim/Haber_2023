# This script takes the climate vars from the regional reanalysis and converts 
# the variables into the correct units for SEM. Compared the annual values with 
# the ameriflux UMBS observations yields some surprising dependencies which 
# which I do not think are from the unit conversion process but from the actual 
# inputs. 

# 0. Set Up --------------------------------------------------------------------
library(assertthat)
library(dplyr)
library(ncdf4)
library(lubridate)

NARR_MET <- here::here("met", "NARR-ED2")
WRITE_TO <- here::here("met", "SEM_3hrly")
dir.create(WRITE_TO, showWarnings = FALSE)

# Mapping df of the month name and the annual order
MONTH_number_mapping <- data.frame(month = toupper(month.abb), 
                                   order = c("01", "02", "03", "04", "05", "06", 
                                             "07", "08", "09", "10", "11", "12"))


# Parse the month name out from the file name 
# Args 
#   l: path to a data file 
# Return: character of the month list 
parse_month <- function(l){
  YYYY_MM <- gsub(x = basename(l), pattern = ".h5", replacement = "")
  out <- substr(YYYY_MM, start = 5, stop = 8 )
  return(out)
}


# Parse the year out from the file name 
# Args 
#   file: the file name of the netcdf 
# Return: character of the year
parse_year <- function(file){
  out <- substr(x = basename(file), start = 1, stop = 4)
  return(out)
}


# Sort the netcdf files for a single year by the month 
# Args 
#   file_list: list of data files for a single year 
# Return: list of data files for a single year sorted by the month 
arrange_files_by_month <- function(file_list){
  
  assert_that(length(file_list) == 12)
  
  df_file_month <- data.frame(file = file_list, month = parse_month(file_list))
  joined_df <- dplyr::inner_join(df_file_month, MONTH_number_mapping, by = "month")
  sorted_df <- arrange(joined_df, order)
  out <- sorted_df$file 
  return(out)
}


# Convert values from deg K to deg C 
# Args 
#     vals: numeric vector of values in K 
# Returns: numeric vector of temp values in C
convert_temp <- function(vals){
  out <- vals - 273
  return(out)
}


# Convert precip values from kg/m^2/s to mm 
# Args 
#   vals: numeric vector of prate in kg/m^2/s 
# Return: numeric vector of the precip in mm 
convert_prate <- function(vals){
  
  # 1 kg/m^2 = 1 mm of rainfall therefore to convert form 
  # kg/m^2/s to mm need to muliply by the number of seconds. 
  # since the NARR data is 3-hourly calculate the number of seconds 
  # in 3 hours 
  # 60 seconds * 60 min * 3 hours
  seconds <- 60 * 60 * 3
  out <- vals * seconds
  return(out)
}


# Calculate the in situ vapor pressure [Pa]
# Args 
#   nc: an open netcdf file
# Return: vector of the in situ vapor pressure [Pa] based on the total pressure 
# and specific humidity
get_insituVP <- function(nc){
  assert_that(class(nc) ==  "ncdf4")
  # Following the https://vortex.plymouth.edu/~stmiller/stmiller_content/Publications/AtmosRH_Equations_Rev.pdf
  p <- ncvar_get(nc, "pres")
  q <- ncvar_get(nc, "sh")
  etta <- 0.622
  vp <- (-q * p) / (-q - etta * q - etta)
  return(vp)
}


# Calculate the saturation vapor pressure [Pa]
# Args 
#   nc: an open netcdf file 
# Return: vector of the saturation vapor pressure [Pa] based on air temperature
get_SVP <- function(nc){
  assert_that(class(nc) ==  "ncdf4")
  # Based on http://cronklab.wikidot.com/calculation-of-vapour-pressure-deficit
  temp <- convert_temp(ncvar_get(nc, "tmp"))
  SVP <- 610.7*10^(7.5*temp/(237.3+temp))
  return(SVP)
}


# Calculate the VPD [kPa] based on climate conditions 
# Args 
#   nc: an open netcdf file
# Return: vector of the vapour-pressure deficit [kPa]
get_VPD <- function(nc){
  insituVP <- get_insituVP(nc)
  SVP <- get_SVP(nc)
  VPD <- (SVP - insituVP)/1000
  return(VPD)
}


# Calculate the PAR μmol∙m-2∙s-1
# Args 
#   nc: an open netcdf file
# Return: PAR [μmol∙m-2∙s-1]
get_PAR <- function(nc){
  total_visible <- ncvar_get(nc, "vbdsf") + ncvar_get(nc, "vddsf") 
  # https://www.horti-growlight.com/en-gb/horticulture-grow-light-conversion-tools
  out <- total_visible * 2.15
  return(out)
}


# Convert temperature from K to C 
# Args 
#   vals: numeric vector of temperature values
# Return: numeric vector of temperature in deg C 
convert_K_C <- function(vals){
  out <- vals - 273.15
  return(out)
}


# Function that constructs a data frame of the time series 
# Args 
#   nc_file: path to the netcdf file name which contains information about the month and day 
# Return: data frame containing date information 
get_date_df <- function(nc_file){
  
  # Parse information out from the netcdf file name
  m_name <- parse_month(nc_file)
  yr <- parse_year(nc_file)
  m_num <- MONTH_number_mapping[MONTH_number_mapping$month == m_name, "order"]
  
  # Construct a sequences of hourly time stamps for the month
  date <- as.Date(paste0(yr, "-", m_num, "-01"), "%Y-%m-%d")
  num_hours <- lubridate::days_in_month(date) * 24 # The number of hours in the month in question
  #num_hours <- 30 * 24
  ts <- seq(from = as.POSIXct(paste0(yr, "-", m_num, "-01 00:00")), 
            length.out = num_hours, by = 3600) 
  
  # Subset the hourly time series to be every 3 hours, this matches the resolution of 
  # the North American Regional Reanalysis (NARR; 3-hourly, 1979-2019)
  index_3_hrly <- seq(from = 1, to = num_hours, by = 3)
  ts <- ts[index_3_hrly]
  
  # Create a dataframe that contains information about the time stamp and other 
  # information. Because sometimes werid stuff happens with the lubridate. 
  out <- data.frame(time = ts) #, 
                    # date = date(ts), 
                    # year = year(ts), 
                    # month = month(ts), 
                    # day = day(ts), 
                    # hour = hour(ts), 
                    # minute = minute(ts))
  return(out)
}


# Function that extracts the needed met data and converts it to the appropriate 
# untis for the SEM 
# Args 
#   nc_file: path to the netcdf file to process
# Return: data frame of the met data! 
get_1_month <- function(nc_file){
  nc <- nc_open(nc_file)
  
  temp_C <- convert_K_C(ncvar_get(nc, "tmp")) # temperature 
  precip_mm <- convert_prate(ncvar_get(nc, "prate")) # precipitation  
  vpd_kPa <- get_VPD(nc) # VPD 
  PAR_umolms <- get_PAR(nc) # PAR
  
  data_df <- data.frame(PAR = PAR_umolms, 
                        temp = temp_C, 
                        VPD = vpd_kPa, 
                        precip = precip_mm)
  time_df <- get_date_df(nc_file)
  out_df <- cbind(time_df, data_df)
  return(out_df)
}



# 1. Process -------------------------------------------------------------------
# TODO will need to interpolate data for 30min resolution.... 
years <- 1979:2019
ofiles <- unlist(lapply(years, function(yr){
  print(yr)
  file_list <- list.files(path = NARR_MET, pattern =  as.character(yr), 
                          full.names = TRUE)
  data_files <- arrange_files_by_month(file_list)
  yr_1 <- do.call(what = "rbind", lapply(X = data_files, FUN = get_1_month))
  ofile <- file.path(WRITE_TO, paste0("3hrly_met_converted_", yr, ".csv"))
  write.csv(yr_1, file = ofile, row.names = FALSE)  
  return(ofile)
}))

