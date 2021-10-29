

# Created 18.10.2021

# Clean the mortality file in Italy. 

#-----------------------------------------------------------------

# One can download the mortality data for 2015-2020 at 
# https://www.istat.it/it/archivio/240401. We selected the file that includes
# deaths till end of January. 



setwd("E:/Postdoc Imperial/Projects/Spatial DLNMs/data/")


installpack <- FALSE


if(installpack){
  install.packages(c("readr", "dplyr", "tidyr", "readxl", "sf", "stringr"))
}



library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(sf)
library(stringr)


deaths <- read_csv("comuni_giornaliero_31gennaio21.csv")

# subset the dataset
deaths %>% select_at(
  vars("PROV", "NOME_PROVINCIA", "CL_ETA", "GE",
                  paste0("M_", 11:21), 
                  paste0("F_", 11:21))
  ) -> deaths



# Change to long format
deaths <- gather(deaths, agesex, deaths, M_11:F_21, factor_key=TRUE)
deaths %>% mutate(
  sex = substr(agesex, start = 1, stop = 1),
  year = as.numeric(paste0("20",substr(agesex, start = 3, stop = 4)))
) -> deaths

deaths$agesex <- NULL

# Fix the age. The CL_ETA is the age variable denoting the following age groups:

# 0=0
# 1=1-4
# 2=5-9
# 3=10-14
# 4=15-19
# 5=20-24
# 6=25-29
# 7=30-34
# 8=35-39
# 9=40-44
# 10=45-49
# 11=50-54
# 12=55-59
# 13=60-64
# 14=65-69
# 15=70-74
# 16=75-79
# 17=80-84
# 18=85-89
# 19=90-94
# 20=95-99
# 21=100+
# see also https://www.istat.it/it/archivio/240401


# Fix the date
t_0 <- Sys.time()
deaths %>% mutate(
  date = paste0(year, "-", 
                substr(GE, start = 1, stop = 2), "-", 
                substr(GE, start = 3, stop = 4))
) %>% mutate(date = as.Date(date)) -> deaths
t_1 <- Sys.time()
t_1 - t_0


deaths <- deaths[!is.na(deaths$date),] # the NAs are "false" leap years
deaths$deaths <- as.numeric(deaths$deaths)

# remove also 2021
deaths %>% filter(year != 2021) -> deaths
# sum(deaths$deaths) # 6427268

# calculate the prevalence by age, sex and year
deaths %>% select(CL_ETA, sex, year, deaths) %>% 
  group_by(CL_ETA, sex, year) %>% 
    summarise(deaths = sum(deaths)) -> forexpected

forexpected %>% filter(year != 2021) -> forexpected
summary(forexpected)
# sum(forexpected$deaths) # 6427268

# expand.grid(
#   sex = c("male", "female"), 
#   ageg = 0:21, 
#   year = 2011:2020
# ) -> expgrid

# correct



# Calculate expected
# load population 
load("pop11_20_final.RData")
sum(pop11_20$pop)

# expand.grid(
#   PROV = unique(pop11_20$Code), 
#   sex = c("male", "female"), 
#   ageg = 0:21, 
#   year = 2011:2020
# ) -> expgrid

# correct

forexpected$sex[forexpected$sex %in% "F"] <- "female"
forexpected$sex[forexpected$sex %in% "M"] <- "male"
pop11_20$Age <- as.numeric(as.character(pop11_20$Age))


pop11_20 %>% group_by(Age, sex, year) %>% 
  summarize(pop = sum(pop)) -> pop.exp

pop.exp <- left_join(pop.exp, forexpected, by = c("year" = "year", "sex" = "sex", "Age" = "CL_ETA"))
pop.exp$prev <- pop.exp$deaths/pop.exp$pop
pop.exp$pop <- NULL

# good now merge these things
pop11_20 <- left_join(pop11_20, pop.exp, by = c("year" = "year", "sex" = "sex", "Age" = "Age"))
pop11_20$expected <- pop11_20$prev*pop11_20$pop
# sum(pop11_20$expected) # 6427268

pop11_20 %>% group_by(year, Province) %>% 
  summarize(expected = sum(expected)) -> expected11_20
table(expected11_20$Province)

# aggregate deaths too

deaths %>% group_by(NOME_PROVINCIA, date) %>% 
  summarise(deaths = sum(deaths)) -> age.deaths

age.deaths$year <- as.numeric(format(age.deaths$date, "%Y"))

table(age.deaths$NOME_PROVINCIA)
table(expected11_20$Province)

# fix the names
age.deaths$NOME_PROVINCIA[startsWith(age.deaths$NOME_PROVINCIA, "Forl")] = "Forli'-Cesena"
age.deaths$NOME_PROVINCIA[startsWith(age.deaths$NOME_PROVINCIA, "Valle")] = "Aosta"
age.deaths$NOME_PROVINCIA[startsWith(age.deaths$NOME_PROVINCIA, "Bolzano")] = "Bolzano"
age.deaths$NOME_PROVINCIA[startsWith(age.deaths$NOME_PROVINCIA, "Massa")] = "Massa Carrara"
age.deaths$NOME_PROVINCIA[startsWith(age.deaths$NOME_PROVINCIA, "Reggio")] = "Reggio di Calabria"


# and bring them together
age.deaths <- left_join(age.deaths, expected11_20, by = c("NOME_PROVINCIA" = "Province", "year" = "year"))
summary(age.deaths)

# assuming that the expected is constant per year.
age.deaths %>% group_by(year, NOME_PROVINCIA) %>% tally() %>% 
  select(year, n) %>% 
  filter(!duplicated(year)) %>% 
  left_join(age.deaths, .) -> age.deaths

age.deaths$expected <- age.deaths$expected/age.deaths$n

saveRDS(age.deaths, file = "deathsItaly11_20")


# 
# 
# 
# # Now link the findata with temperature, holidays and population.
# 
# temperature <- readRDS("Output/TemperatureWeeklyItaly")
# holidays <- readRDS("Output/holiday_df")
# population <- readRDS("Output/pop_weekly")
# 
# # for the linkage I will need the NUTS318CD, ie the acronyms of the NUTS3 regions.
# shp <- read_sf("ProvCM01012020_g_WGS84.shp")
# linkage <- data.frame(ID = shp$COD_PROV, NUTS318CD = shp$SIGLA)
# linkage$ID <- str_pad(linkage$ID, 3, pad = "0")
# 
# findata <- left_join(findata, linkage, by = c("PROV" = "ID"))
# 
# 
# findata <- left_join(findata, temperature[,-1], by = c("EURO_LABEL" = "EURO_LABEL", "NUTS318CD" = "SIGLA"))
# 
# holidays$hol <- 1
# holidays$Data <- holidays$Week <- holidays$Year <- NULL
# holidays <- holidays[!duplicated(holidays$EURO_LABEL),]
# findata <- left_join(findata, holidays, by = c("EURO_LABEL" = "EURO_LABEL"))
# findata$hol[is.na(findata$hol)] <- 0
# 
# 
# population$sex[population$sex %in% "male"] <- "M"
# population$sex[population$sex %in% "female"] <- "F"
# findata <- left_join(findata, population, by = c("ageg" = "age", 
#                                                  "sex" = "sex", 
#                                                  "NUTS318CD" = "NUTS318CD", 
#                                                  "EURO_LABEL" = "EURO_LABEL"))
# 
# # and store the findata. 
# 
# saveRDS(findata, file = "Output/findata")



######################################################################################
######################################################################################
######################################################################################
######################################################################################




