



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


setwd("E:/Postdoc Imperial/Projects/Spatial DLNMs/data/")

# read the files
relativehumidity <- nc_open("rh_eu.nc")
extr.rh <- ncvar_get(relativehumidity, varid="r2")

tmp.rstr <- raster("rh_eu.nc")

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
) # approximately 10h



TMP <- GetRH



GetRH <- do.call(rbind, GetRH)

GetRH %>% 
  mutate(ID = group_indices(., lon, lat)) -> GetRH

saveRDS(GetRH, file = "E:/Postdoc Imperial/Projects/Spatial DLNMs/data/GetRHEU07_19")


GetRH <- readRDS("E:/Postdoc Imperial/Projects/Spatial DLNMs/data/GetRHEU07_19")
tmp.latlon <- GetRH[!duplicated(GetRH$ID),]

# tmp.latlon %>% mutate(lon = if_else(lon>180, lon-360, lon)) -> tmp.latlon

# Now I need to project it on the IT grid
plot(tmp.latlon[,c(1:2)])
# Now we need the shp in the UK

mun <- read_sf("C:/Users/gkonstan/Desktop/exploratory analysis/Effect modifiers/LTLA 2015/Local_Authority_Districts_(December_2015)_Boundaries.shp")

crs.lcc <- "+proj=lcc +lat_1=48 +lat_2=48 +lat_0=48 +lon_0=8 +x_0=3101971.52403412
+y_0=2994156.36404286 +a=6371229 +b=6371229 +to_meter=11000"


crs.lcc <- "+proj=lcc +lat_1=48 +lat_2=48 +lat_0=48 +lon_0=8 +x_0=3101971.52403412
+y_0=2994156.36404286 +a=6371229 +b=6371229 +to_meter=11000 +to +proj=lonlat
+ellps=WGS84 +datum=WGS84"


# make sure shp and temperature file are in the same projection

DT_sf <- st_as_sf(tmp.latlon[, c("lon", "lat")], coords = c("lon", "lat"), crs = crs.lcc)
DT_sf <- st_transform(DT_sf, crs = st_crs(mun))

mun %>% filter(str_detect(lad15cd, "^E")) %>% 
  st_join(DT_sf, .) %>% 
  mutate(ID = tmp.latlon$ID) %>% 
  filter(!is.na(objectid)) -> tmpjoin

tmpjoin %>% st_coordinates() %>% as.data.frame() %>% mutate(ID = tmpjoin$ID) -> tmpjoin

# and merge back to the GetRH
GetRH <- left_join(GetRH, tmpjoin, by = c("ID" = "ID"))
GetRH <- GetRH[!is.na(GetRH$X),]
gc()

GetRH$lon <- GetRH$lat <- NULL


select.date <- "2007-06-01"
GetRH <- GetRH[,c("ID", "X", "Y", "date", "relativehumidity")]
GetRH %>% filter(date == select.date) %>% 
  ggplot() + geom_point(aes(x = X, y = Y, col = relativehumidity), size = 4) + 
  scale_colour_viridis_c(alpha = 0.6)
  
# test it with the relative humidity of the file you created

rh_mod <- readRDS("C:/Users/gkonstan/Desktop/exploratory analysis/SSRS/move_2/RH_Predictions2007_2019_10km_lags")

rh_mod %>% filter(days == select.date) %>% 
  ggplot() + geom_point(aes(x = coords.x1, y = coords.x2, col = RH_median), size = 4) + 
  scale_colour_viridis_c(alpha = 0.6, limits = c(65, 100)/100)






GetRH %>% filter(date == select.date) -> tt






GetRH <- GetRH[,c("ID", "lon", "lat", "date", "relativehumidity")]
GetRH %>% filter(date == select.date) %>% 
  ggplot() + geom_point(aes(x = X, y = Y, col = relativehumidity), size = 4) + 
  scale_colour_viridis_c(limits = c(65, 100))



GetRH %>% filter(date == select.date) %>% 
  mutate(lon = if_else(lon>180, lon-360, lon)) %>% 
  ggplot() + geom_point(aes(x = lon, y = lat, col = relativehumidity), size = 4) + 
  scale_colour_viridis_c(alpha = 0.6)



png("tmpplot.png", width = 20, height = 20, units = "cm", res = 300)
tt %>% mutate(lon = if_else(lon>180, lon-360, lon)) %>% 
  ggplot() + geom_point(aes(x = lon, y = lat, col = relativehumidity), size = 3) + 
  scale_colour_viridis_c(alpha = 0.6, limits = c(65, 100))
dev.off()

