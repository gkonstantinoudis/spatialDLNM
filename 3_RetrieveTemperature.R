


# Created 07.10.2021

# Clean and download temperature


#---------------------------------------------------------------------------------

setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/TutorialExcess/")

installpack <- FALSE




# Step 1. Download temperature data from ERA5

# install required packages
if(installpack){
  install.packages(c("ecmwfr"))
}


# load packages
library(ecmwfr)

# You need to create an account here https://cds.climate.copernicus.eu/cdsapp#!/home, 
# and once you are ok and logged in, click on your name on the top right next to logout
# and retrieve the information about the API key.


cds.key <- "Insert_your_CDS_API_KEY_here"
wf_set_key(user = "Insert_your_CDS_UID_here", key = cds.key, service = "cds")

request <- list(
  dataset_short_name = "reanalysis-era5-single-levels",
  product_type   = "reanalysis",
  format = "netcdf",
  variable = "2m_temperature",
  year = c("2015", "2016", "2017", "2018", "2019", "2020"),
  month = c(paste0("0", 1:9), 10:12),
  day = "16",
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", 
           "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", 
           "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  # area is specified as N, W, S, E
  area = c(48, 6, 34, 20),
  target = "temperature2015_2020_Italy.nc"
)

file <- wf_request(user = "52967",
                   request = request,
                   transfer = TRUE,
                   path = "~",
                   time_out = 3600*12,
                   verbose = TRUE)


# and you will get a temperature_Italy.nc file on your working directory. 



# Alternatively one can use this link https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview
# select download data, and make the following selection: Temperature: 2m temperature, Years: 2015-2020, Months: Select all, Days: All, Time: Select all, 
# and for the geographical area, selet sub-region extraction and use 48, 6, 34, 20 specified as N, W, S, E. Store this file on
# your working directory as temperature_Italy.nc.



# The temperature_Italy.nc file is also provided for download here: 
# https://drive.google.com/drive/folders/1H7F4PuiLlcRwWtbmsJAGPEWJtLu30BN6?usp=sharing







# Step 2. Clean the temperature file

if(installpack){
  install.packages(c("ncdf4", "plyr", "tidyr", "pbapply", "sf", 
                     "tidyverse", ",maptools", "lctools", "raster", 
                     "lubridate", "spdep", "FNN", "readxl", "patchwork"))
}



library(ncdf4)
library(plyr)
library(tidyr)
library(pbapply)
library(sf)
library(tidyverse)
library(maptools)
library(lctools)
library(raster)
library(lubridate)
library(spdep)
library(FNN)
library(patchwork)
library(dplyr)
library(readxl)

setwd("E:/Postdoc Imperial/Projects/Spatial DLNMs/data/")


# read the files
temperature <- nc_open("temperatureItaly.nc")
extr.tmp <- ncvar_get(temperature, varid="t2m")

# extract space and time
lon <- ncvar_get(temperature,"longitude")
lat <- ncvar_get(temperature,"latitude")
hour <- ncvar_get(temperature,"time")
# hours since 1900-01-01
hour_tr <- as.POSIXct(hour*3600, origin="1900-01-01 00:00")
hour_tr <- format(as.POSIXct(hour_tr,format='%Y-%m-%d %H:%M:%S GMT'),format='%Y-%m-%d')
length(hour_tr)

dat <- data.frame(start = seq(from = 1, to = length(hour_tr), by = 24), 
                  stop = seq(from = 24, to = length(hour_tr), by = 24))

un.hour <- unique(hour_tr)
un.hour <- un.hour[order(un.hour)]
dat$date <- un.hour



# function to retrieve daily mean
DailyMean <- function(start, stop, date){
  
  tmp <- aaply(extr.tmp[,,start:stop], .margin = c(1,2), .fun = function(Y) mean(Y-273.15))
  tmp <- as.data.frame(tmp)
  
  colnames(tmp) <- lat
  rownames(tmp) <- lon
  
  mat2store <- expand.grid(lon, lat)
  colnames(mat2store) <- c("lon", "lat")
  mat2store <- cbind(mat2store, as.vector(as.matrix(tmp)))  
  
  mat2store <- as.data.frame(mat2store)
  colnames(mat2store)[3] <- "temperature"
  
  mat2store <- as.data.frame(mat2store)
  mat2store$date <- as.Date(date)
  
  mat2store <- mat2store[complete.cases(mat2store$temperature),]
  
  return(mat2store)
}


GetTemperature <- 
  pbapply(dat, 1, function(X){
    
    return(DailyMean(start = X[1], stop = X[2], date = X[3]))
    
}
) # approximately 2h



GetTemperature <- do.call(rbind, GetTemperature)
GetTemperature %>% 
  mutate(ID = group_indices(., lon, lat)) -> GetTemperature

# saveRDS(GetTemperature, file = "tmp_it_211020")
# GetTemperature <- readRDS("tmp_it_211020")




# Now we need the shp in Italy.
mun <- read_sf("ProvCM01012020_g_WGS84.shp")

# make sure shp and temperature file are in the same projection
DT_sf <- st_as_sf(GetTemperature[, c("lon", "lat")], coords = c("lon", "lat"), crs = 4326)
DT_sf <- st_transform(DT_sf, crs = st_crs(mun))
DT_sf <- st_coordinates(DT_sf)

GetTemperature <- cbind(GetTemperature, DT_sf)


# store it 
# saveRDS(GetTemperature, file = "E:/Postdoc Imperial/Projects/COVID19 Greece/data/temperature/Gettmp_it_211020")
# GetTemperature <- readRDS("E:/Postdoc Imperial/Projects/COVID19 Greece/data/temperature/GetTemperature_IT_210420")



# Now I need to overlay it on the shp and take the mean by municipality and week
loopID <- unique(GetTemperature$date)
list.loop <- list()
list.plot <- list()

mun$IDSpace <- 1:nrow(mun)

for(i in 1:length(loopID)){
  
  print(i)
  tmp <- GetTemperature %>% filter(date %in% loopID[i])
  tmp_sf <- st_as_sf(tmp, coords = c("X", "Y"), crs = st_crs(mun))
  tmp_sf$X <- tmp$X
  tmp_sf$Y <- tmp$Y
  
  tmp_stjoin <- st_join(mun, tmp_sf)
  
  tmp_stjoin <- as.data.frame(tmp_stjoin)
  tmp_stjoin$geometry <- NULL
  
  # and calculate mean temperature of points that fall in a particular municipality 
  tmp_stjoin %>% group_by(IDSpace) %>% 
    mutate(mean.temp = mean(temperature, na.rm = TRUE)) %>% 
    filter(!duplicated(IDSpace)) -> tmp_stjoin
  
  tmp_stjoin <- tmp_stjoin[,c("IDSpace", "SIGLA", "date", "mean.temp")]
  tmp_stjoin$IDSpace <- as.character(tmp_stjoin$IDSpace)

  list.loop[[i]] <- tmp_stjoin
}


loop.df <- do.call(rbind, list.loop)

# The temperature file clean
saveRDS(loop.df, file = "TemperatureDailyItaly_11_20")






# Code for Figure 1

select.date <- "2019-08-05"
loop.df %>% filter(date == select.date) %>% 
  left_join(mun, ., by = ("SIGLA" = "SIGLA")) -> tmp_mun

ggplot()  + theme_light() + 
  geom_sf(data = tmp_mun, aes(fill = mean.temp), col = "grey44") + 
  ylab("") + xlab("") + 
  scale_fill_viridis_c(name = "") + 
  ggtitle(paste0("Mean daily temperature during in ", select.date)) -> p1

p1




##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
