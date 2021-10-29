



# Created 20.10.2021


# Relative humidity in Europe 2007-2019



################################################################################


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
library(data.table)



setwd("E:/Postdoc Imperial/Projects/Spatial DLNMs/data/")

# read the files
# The data was downloaded from: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-uerra-europe-single-levels?tab=form
# and the selection was UERRA-HARMONIE, 2010-2019, all months, all days, all times, NetCDF.

relativehumidity <- nc_open("rh_eu.nc")
extr.rh <- ncvar_get(relativehumidity, varid="r2")

# extract space and time
lon <- ncvar_get(relativehumidity,"longitude")
lat <- ncvar_get(relativehumidity,"latitude")

hour <- ncvar_get(relativehumidity,"time")
# hours since 1900-01-01
hour_tr <- as.POSIXct(hour, origin="1970-01-01")
hour_tr <- format(as.POSIXct(hour_tr,format='%Y-%m-%d %H:%M:%S GMT'),format='%Y-%m-%d')
length(hour_tr)

dat <- data.frame(start = seq(from = 1, to = length(hour_tr), by = 4), 
                  stop = seq(from = 4, to = length(hour_tr), by = 4))

un.hour <- unique(hour_tr)
un.hour <- un.hour[order(un.hour)]
dat$date <- un.hour


# function to retrieve daily mean
DailyMean <- function(start, stop, date){
  
  tmp <- aaply(extr.rh[,,start:stop], .margin = c(1,2), .fun = function(Y) mean(Y))
  mat2store <- cbind(as.vector(lon), as.vector(lat), as.vector(tmp))
  colnames(mat2store) <- c("lon", "lat", "relativehumidity")
  mat2store <- as.data.frame(mat2store)
  mat2store$date <- as.Date(date)
  
  mat2store <- mat2store[complete.cases(mat2store$relativehumidity),]
  
  return(mat2store)
}


GetRH <- 
  pbapply(dat, 1, function(X){
    
    return(DailyMean(start = X[1], stop = X[2], date = X[3]))
    
  }
) 


TMP <- GetRH
GetRH <- do.call(rbind, GetRH)

# saveRDS(GetRH, file = "GetRHEU07_19")
# GetRH <- readRDS("GetRHEU07_19")
rm(relativehumidity, lat, lon, dat, TMP, extr.rh)
gc()

# create an ID
GetRH$ID <- paste0(GetRH$lon, GetRH$lat)
GetRH$ID <- as.factor(GetRH$ID)
GetRH$ID <- as.numeric(GetRH$ID)


tmp.latlon <- GetRH[!duplicated(GetRH$ID),]

# Now I need to project it on the IT grid
mun <- read_sf("E:/Postdoc Imperial/Projects/Spatial DLNMs/data/ProvCM01012020_g_WGS84.shp")

# this projection string is retrieved from the documentation of UERRA-HARMONIE
crs.lcc <- "+proj=lcc +lat_1=48 +lat_2=48 +lat_0=48 +lon_0=8 +x_0=3101971.52403412
+y_0=2994156.36404286 +a=6371229 +b=6371229 +to_meter=11000 +to +proj=lonlat
+ellps=WGS84 +datum=WGS84"


# make sure shp and temperature file are in the same projection
DT_sf <- st_as_sf(tmp.latlon[, c("lon", "lat")], coords = c("lon", "lat"), crs = crs.lcc)
DT_sf <- st_transform(DT_sf, crs = st_crs(mun))

mun %>% 
  st_join(DT_sf, .) %>% 
  mutate(ID = tmp.latlon$ID) %>% 
  filter(!is.na(SIGLA)) -> tmpjoin


tmpjoin %>% st_coordinates() %>% as.data.frame() %>% mutate(ID = tmpjoin$ID) -> tmpjoin

# and merge back to the GetRH
GetRH <- left_join(GetRH, tmpjoin, by = c("ID" = "ID"))
gc()
GetRH <- GetRH[!is.na(GetRH$X),]
gc()
GetRH$lon <- GetRH$lat <- NULL

# saveRDS(GetRH, file = "E:/Postdoc Imperial/Projects/Spatial DLNMs/data/GetRHIT_10_19")

# A quick check
# select.date <- "2011-06-01"
# GetRH <- GetRH[,c("ID", "X", "Y", "date", "relativehumidity")]
# GetRH %>% filter(date == select.date) %>% 
#   ggplot() + geom_point(aes(x = X, y = Y, col = relativehumidity), size = 4) + 
#   scale_colour_viridis_c(alpha = 0.6)
  

GetRH <- readRDS("GetRHIT_10_19")

# Now we need the shp in Italy.
mun <- read_sf("ProvCM01012020_g_WGS84.shp")


# Now I need to overlay it on the shp and take the mean by municipality and week
loopID <- unique(GetRH$date)
list.loop <- list()
list.plot <- list()

mun$id_space <- 1:nrow(mun)

for(i in 1:length(loopID)){
  
  print(i)
  tmp <- GetRH %>% filter(date %in% loopID[i])
  tmp_sf <- st_as_sf(tmp, coords = c("X", "Y"), crs = st_crs(mun))
  tmp_sf$X <- tmp$X
  tmp_sf$Y <- tmp$Y
  
  tmp_stjoin <- st_join(mun, tmp_sf)

  tmp_stjoin <- as.data.frame(tmp_stjoin)
  tmp_stjoin$geometry <- NULL
  
  # and calculate mean temperature of points that fall in a particular municipality 
  tmp_stjoin %>% group_by(id_space) %>% 
    mutate(mean.rh = mean(relativehumidity, na.rm = TRUE)) %>% 
    filter(!duplicated(id_space)) -> tmp_stjoin
  
  tmp_stjoin <- tmp_stjoin[,c("id_space", "SIGLA", "date", "mean.rh")]
  tmp_stjoin$id_space <- as.character(tmp_stjoin$id_space)
  
  list.loop[[i]] <- tmp_stjoin
}


loop.df <- do.call(rbind, list.loop)

# saveRDS(loop.df, file = "E:/Postdoc Imperial/Projects/Spatial DLNMs/data/RHIT_10_19")

########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################