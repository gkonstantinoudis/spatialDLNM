


# Created 29.10.2021


# Bring together part 1


#############################################################

library(dplyr)
library(sf)
library(lubridate)

setwd("E:/Postdoc Imperial/Projects/Spatial DLNMs/data/")


deaths <- readRDS("deathsItaly11_20")
holidays <- readRDS("holiday_df")
relativehumidity <- readRDS("RHIT_10_19")
temperature <- readRDS("TemperatureDailyItaly_11_20")
mun <- read_sf("ProvCM01012020_g_WGS84.shp")


# I will clean this data to focus on the summer months. 
holidays$hol <- 1
deaths <- left_join(deaths, holidays, by = c("date" = "Data"))
deaths$hol[is.na(deaths$hol)] <- 0

# lets take 6 hottest months: May-Sept
deaths %>% mutate(year = year(date), month = month(date)) %>% filter((month %in% c(5:9)) & (year %in% c(2011:2018))) -> deaths
relativehumidity %>% mutate(year = year(date), month = month(date)) %>% filter((month %in% c(5:9)) & (year %in% c(2011:2018))) -> relativehumidity
temperature %>% mutate(year = year(date), month = month(date)) %>% filter((month %in% c(5:9)) & (year %in% c(2011:2018))) -> temperature

# excellent they're the same size

mun %>% as.data.frame() %>% mutate(geometry := NULL) %>% dplyr::select(SIGLA, DEN_UTS) -> link_tab

deaths <- left_join(deaths, link_tab, by = c("NOME_PROVINCIA" = "DEN_UTS"))
deaths <- left_join(deaths, relativehumidity[,c("SIGLA", "date", "mean.rh")], by = c("SIGLA" = "SIGLA", "date" = "date"))
deaths <- left_join(deaths, temperature[,c("SIGLA", "date", "mean.temp")], by = c("SIGLA" = "SIGLA", "date" = "date"))

saveRDS(deaths, file = "Findata")


#############################################################
#############################################################
#############################################################
