library(readr)
library(sf)
library(rgdal)
library(dplyr)
library(ggplot2)

library(sp)
#library(raster)

# Impute the NAs by first order neighbors  --------------------------------


# load Italy shapefile 
ITA_shp = readOGR("data/geography/Limiti01012021_g/ProvCM01012021_g/ProvCM01012021_g_WGS84.shp")
plot(ITA_shp)
ITA_shp <- st_as_sf(ITA_shp)



# Determine neighborhood/adjacency of provinces
W.nb <- spdep::poly2nb(ITA_shp)


# Impute missing values for areas based on the first order neighbors
# !!! The imputation step is performed indipendently for each day 

impute_function<- function(date_idx, pollutant, W.nb, shp_file){
  
  ## select date
  pollutant_small <- pollutant %>% filter(date == date_idx)
  
  ## find names of areas with missing values
  na_names <- pollutant_small %>% filter(is.na(value)) %>% dplyr::select(provincia) %>% c()
  
  
  if(length(na_names$provincia)>0){ # check if there are no missing 
    
    for(x in na_names$provincia){
      # use the W.nb object to find the neigbors's names
      ngb_idx   = W.nb[[which(shp_file$DEN_UTS == x)]]
      ngb_names <- shp_file$DEN_UTS[ngb_idx]
      
      # replace missing data with mean of neighbors
      pollutant_small[pollutant_small$provincia == x, "value"] <- mean(pollutant_small[pollutant_small$provincia%in%ngb_names, "value"], na.rm = TRUE )
      pollutant_small[pollutant_small$provincia == x, "date"] <- date_idx
    }
    
  }
  
  return(pollutant_small)
  
}





# Pm2.5 -------------------------------------------------------------------


## load pm25
pm25_raw <- read_csv("data/SESERIE STORICHE DAY/pm25input.csv")

# variable pollutant_fk is useless, no need to group by it 
table(pm25_raw$pollutant_fk)


# assign to each province the mean value of all the monitors in the province
pm25 <- pm25_raw %>% group_by(provincia, date) %>% summarise(value = mean(value, na.rm = T)) %>% ungroup() 

# make sure that there are no gaps in the data 
date_prov_grid <- with(data = pm25, expand.grid(provincia = unique(ITA_shp$DEN_UTS), date = unique(date) ))
pm25 <- left_join(date_prov_grid, pm25)





## Graphical checks
foo_check <- pm25 %>% group_by(provincia) %>% summarise(value = mean(value, na.rm = T)) %>% ungroup()
left_join(ITA_shp, foo_check, by = c("DEN_UTS" = "provincia")) %>% ggplot() + geom_sf(aes(fill = value), colour = NA) +
  scale_fill_viridis_c() + ggtitle("Average PM2.5 over the whole study period + 2020") +
  theme_minimal()


num_na <- pm25 %>% group_by(date) %>% summarise(na_num = sum(is.na(value)))
summary(num_na)
ggplot(num_na, aes(x = date, y = na_num)) + geom_line() + theme_minimal() + 
  ylab("number of NAs") + xlab("date") +  ggtitle("Number of Provinces to be imputed for PM2.5")


foo_check <- pm25 %>% group_by(provincia) %>% summarise(value = mean(is.na(value))) %>% ungroup()
left_join(ITA_shp, foo_check, by = c("DEN_UTS" = "provincia")) %>% ggplot() + geom_sf(aes(fill = value), colour = NA) +
  scale_fill_viridis_c() + ggtitle("Prop of NA dates for PM2.5 over the whole study period + 2020") +
  theme_minimal()
rm(foo_check)






pm25 <- pm25 %>% filter(date< "2020-01-01")
summary(pm25)

num_na <- pm25 %>% group_by(date) %>% summarise(na_num = sum(is.na(value)))
summary(num_na)
ggplot(num_na, aes(x = date, y = na_num)) + geom_line() + theme_minimal() + 
  ylab("number of NAs") + xlab("date") + ggtitle("Number of Provinces to be imputed for PM2.5")

foo_check <- pm25 %>% group_by(provincia) %>% summarise(value = mean(value, na.rm = T)) %>% ungroup()
left_join(ITA_shp, foo_check, by = c("DEN_UTS" = "provincia")) %>% ggplot() + geom_sf(aes(fill = value), colour = NA) +
  scale_fill_viridis_c() + ggtitle("Average PM2.5 over the whole study period") +
  theme_minimal()
rm(foo_check)

foo_check <- pm25 %>% group_by(provincia) %>% summarise(value = mean(is.na(value))) %>% ungroup()
left_join(ITA_shp, foo_check, by = c("DEN_UTS" = "provincia")) %>% ggplot() + geom_sf(aes(fill = value), colour = NA) +
  scale_fill_viridis_c() + ggtitle("Prop of NA dates for PM2.5 over the whole study period") +
  theme_minimal()
rm(foo_check)




print(paste("Time window -- from", min(num_na$date), "to", max(num_na$date) ))

print("Check for gaps in the data")
if( (max(num_na$date) - min(num_na$date) + 1) == length(unique(pm25$date))){
  print("There are no missing days")
} else {
  print("Watch out -- there are gaps in your data")
}



impute_function("2016-06-21", pollutant= pm25, W.nb = W.nb, shp_file = ITA_shp)

pm25_clean <- lapply(unique(pm25$date), impute_function,  pollutant= pm25, W.nb = W.nb, shp_file = ITA_shp)
pm25_clean <- do.call("rbind", pm25_clean)
pm25_clean$poll <- "pm25"


saveRDS(pm25_clean, file = "pm25.RDS")




# NO2 ---------------------------------------------------------------------

library(readxl)

# monitor metadata
monitor_meta <- read_excel("data/SESERIE STORICHE DAY/STAZIONI.xlsx")


#list all files in data
data_files <- dir("data/SESERIE STORICHE DAY", pattern = "^no2", full.names = TRUE)


library(readr)

import_no2 <- function(file_path){
  
  no2_temp <- read_csv(file_path, 
                      col_types = cols(station_code = col_character()))
  
  no2_temp2 <- left_join(no2_temp, monitor_meta, by = "station_code")
  no2_temp3 <- no2_temp2 %>% group_by(provincia, date) %>% summarise(value = mean(no2, na.rm = TRUE))
  
  no2_temp3
  
}

no2_raw = lapply(data_files, import_no2)
no2_raw = do.call("rbind", no2_raw)





date_prov_grid <- with(data = no2_raw, expand.grid(provincia = unique(ITA_shp$DEN_UTS), date = unique(date) ))
no2_raw <- left_join(date_prov_grid, no2_raw)

foo_check <- no2_raw %>% group_by(provincia) %>% summarise(value = mean(value, na.rm = T)) %>% ungroup()
left_join(ITA_shp, foo_check, by = c("DEN_UTS" = "provincia")) %>% ggplot() + geom_sf(aes(fill = value), colour = NA) +
  scale_fill_viridis_c() + ggtitle("Average NO2 over the whole study period + 2020") +
  theme_minimal()
rm(foo_check)



num_na <- no2_raw %>% group_by(date) %>% summarise(na_num = sum(is.na(value)))
summary(num_na)
ggplot(num_na, aes(x = date, y = na_num)) + geom_line() + theme_minimal() + 
  ylab("number of NAs") + xlab("date") + ggtitle("Number of Provinces to be imputed for NO2")


no2_raw <- no2_raw %>% filter(date< "2020-01-01")
summary(no2_raw)

num_na <- no2_raw %>% group_by(date) %>% summarise(na_num = sum(is.na(value)))
summary(num_na)
ggplot(num_na, aes(x = date, y = na_num)) + geom_line() + theme_minimal() + 
  ylab("number of NAs") + xlab("date") + ggtitle("Number of Provinces to be imputed for NO2")


foo_check <- no2_raw %>% group_by(provincia) %>% summarise(value = mean(value, na.rm = T)) %>% ungroup()
left_join(ITA_shp, foo_check, by = c("DEN_UTS" = "provincia")) %>% ggplot() + geom_sf(aes(fill = value), colour = NA) +
  scale_fill_viridis_c() + ggtitle("Average NO2 over the whole study period") +
  theme_minimal()
rm(foo_check)


print(paste("Time window -- from", min(num_na$date), "to", max(num_na$date) ))

print("Check for gaps in the data")
if( (max(num_na$date) - min(num_na$date) + 1) == length(unique(no2_raw$date))){
  print("There are no missing days")
} else {
  print("Watch out -- there are gaps in your data")
}



no2_clean <- lapply(unique(no2_raw$date), impute_function,  pollutant= no2_raw, W.nb = W.nb, shp_file = ITA_shp)
no2_clean <- do.call("rbind", no2_clean)
no2_clean$poll <- "no2"


saveRDS(no2_clean, file = "no2.RDS")


# Ozone -------------------------------------------------------------------

o3_raw <- read_csv("data/SESERIE STORICHE DAY/o3input.csv")
table(o3_raw$pollutant_fk)

o3_raw <- o3_raw %>% group_by(provincia, date) %>% summarise(value = mean(o3_max_h_d, na.rm = T)) %>% ungroup() 
date_prov_grid <- with(data = o3_raw, expand.grid(provincia = unique(ITA_shp$DEN_UTS), date = unique(date) ))
o3_raw <- left_join(date_prov_grid, o3_raw)

foo_check <- o3_raw %>% group_by(provincia) %>% summarise(value = mean(value, na.rm = T)) %>% ungroup()
left_join(ITA_shp, foo_check, by = c("DEN_UTS" = "provincia")) %>% ggplot() + geom_sf(aes(fill = value), colour = NA) +
  scale_fill_viridis_c() + ggtitle("Average O3 over the whole study period + 2020") +
  theme_minimal()
rm(foo_check)



num_na <- o3_raw %>% group_by(date) %>% summarise(na_num = sum(is.na(value)))
summary(num_na)
ggplot(num_na, aes(x = date, y = na_num)) + geom_line() + theme_minimal() + 
  ylab("number of NAs") + xlab("date") +  ggtitle("Number of Provinces to be imputed for O3")


o3_raw <- o3_raw %>% filter(date< "2020-01-01")
summary(o3_raw)

num_na <- o3_raw %>% group_by(date) %>% summarise(na_num = sum(is.na(value)))
summary(num_na)
ggplot(num_na, aes(x = date, y = na_num)) + geom_line() + theme_minimal() + 
  ylab("number of NAs") + xlab("date") + ggtitle("Number of Provinces to be imputed for O3")

foo_check <- o3_raw %>% group_by(provincia) %>% summarise(value = mean(value, na.rm = T)) %>% ungroup()
left_join(ITA_shp, foo_check, by = c("DEN_UTS" = "provincia")) %>% ggplot() + geom_sf(aes(fill = value), colour = NA) +
  scale_fill_viridis_c() + ggtitle("Average O3 over the whole study period") +
  theme_minimal()
rm(foo_check)



print(paste("Time window -- from", min(num_na$date), "to", max(num_na$date) ))

print("Check for gaps in the data")
if( (max(num_na$date) - min(num_na$date) + 1) == length(unique(o3_raw$date))){
  print("There are no missing days")
} else {
  print("Watch out -- there are gaps in your data")
}



o3_clean <- lapply(unique(o3_raw$date), impute_function,  pollutant= o3_raw, W.nb = W.nb, shp_file = ITA_shp)
o3_clean <- do.call("rbind", o3_clean)
o3_clean$poll <- "o3"

saveRDS(o3_clean, file = "o3.RDS")


# Pm10 --------------------------------------------------------------------


library(readxl)

#list all files in data
data_files_pm10 <- dir("data/SESERIE STORICHE DAY", pattern = "^pm10", full.names = TRUE)


library(readr)

import_pm10 <- function(file_path){
  
  pm10_temp <- read_csv(file_path, 
                       col_types = cols(station_code = col_character()))
  
  pm10_temp2 <- left_join(pm10_temp, monitor_meta, by = "station_code")
  pm10_temp3 <- pm10_temp2 %>% group_by(provincia, date) %>% summarise(value = mean(pm10, na.rm = TRUE))
  
  pm10_temp3
  
}

pm10_raw = lapply(data_files_pm10, import_pm10)
pm10_raw = do.call("rbind", pm10_raw)





date_prov_grid <- with(data = pm10_raw, expand.grid(provincia = unique(ITA_shp$DEN_UTS), date = unique(date) ))
pm10_raw <- left_join(date_prov_grid, pm10_raw)

foo_check <- pm10_raw %>% group_by(provincia) %>% summarise(value = mean(value, na.rm = T)) %>% ungroup()
left_join(ITA_shp, foo_check, by = c("DEN_UTS" = "provincia")) %>% ggplot() + geom_sf(aes(fill = value), colour = NA) +
  scale_fill_viridis_c() + ggtitle("Average PM10 over the whole study period + 2020") +
  theme_minimal()
rm(foo_check)



num_na <- pm10_raw %>% group_by(date) %>% summarise(na_num = sum(is.na(value)))
summary(num_na)
ggplot(num_na, aes(x = date, y = na_num)) + geom_line() + theme_minimal() + 
  ylab("number of NAs") + xlab("date") +  ggtitle("Number of Provinces to be imputed for PM10")


pm10_raw <- pm10_raw %>% filter(date< "2020-01-01")
summary(pm10_raw)

num_na <- pm10_raw %>% group_by(date) %>% summarise(na_num = sum(is.na(value)))
summary(num_na)
ggplot(num_na, aes(x = date, y = na_num)) + geom_line() + theme_minimal() + 
  ylab("number of NAs") + xlab("date") + ggtitle("Number of Provinces to be imputed for PM10")

foo_check <- pm10_raw %>% group_by(provincia) %>% summarise(value = mean(value, na.rm = T)) %>% ungroup()
left_join(ITA_shp, foo_check, by = c("DEN_UTS" = "provincia")) %>% ggplot() + geom_sf(aes(fill = value), colour = NA) +
  scale_fill_viridis_c() + ggtitle("Average PM10 over the whole study period") +
  theme_minimal()
rm(foo_check)




print(paste("Time window -- from", min(num_na$date), "to", max(num_na$date) ))

print("Check for gaps in the data")
if( (max(num_na$date) - min(num_na$date) + 1) == length(unique(no2_raw$date))){
  print("There are no missing days")
} else {
  print("Watch out -- there are gaps in your data")
}



pm10_clean <- lapply(unique(pm10_raw$date), impute_function,  pollutant= pm10_raw, W.nb = W.nb, shp_file = ITA_shp)
pm10_clean <- do.call("rbind", pm10_clean)
pm10_clean$poll <- "pm10"

saveRDS(pm10_clean, file = "pm10.RDS")




all_pol <- rbind(pm25_clean, pm10_clean, no2_clean, o3_clean)


all_pol %>% filter(provincia == "Verbano-Cusio-Ossola") %>% ggplot(aes(x = date, y = value, color = poll)) + geom_line() + theme_bw() + scale_color_viridis_d() + facet_grid(~poll )
