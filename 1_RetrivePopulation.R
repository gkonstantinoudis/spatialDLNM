


# Created 18.10.2021

# Clean and download population


#---------------------------------------------------------------------------------


setwd("E:/Postdoc Imperial/Projects/Spatial DLNMs/data/")


installpack <- FALSE


if(installpack){
  install.packages(c("tidyverse", "reshape2", "lubridate", "rgdal", "spdep", "readr", "tidyr"))
}





library(tidyverse)
library(reshape2)
library(lubridate)
library(rgdal)
library(spdep)
library(readr)
library(tidyr)





# Population for January 1st 2020 can be downloaded from: http://demo.istat.it/popres/download.php?anno=2020&lingua=ita
# selecting Province on the bottom right of the page. Save this object as POP2020.

pop20 <- read_csv("POP2020.csv")
colnames(pop20) <- pop20[1,]
pop20 <- pop20[-1,]


pop20 = pop20 %>% select(`Codice provincia`, `Provincia`, `Totale Maschi`, `Totale Femmine`, `Età`)


pop20 %>% select(`Codice provincia`, `Provincia`, `Totale Maschi`, `Età`) %>% 
  mutate(sex = "M") %>% 
  rename(Value := `Totale Maschi`) %>% 
  rbind(., 
        pop20 %>% select(`Codice provincia`, `Provincia`, `Totale Femmine`, `Età`) %>% 
          mutate(sex = "F") %>% 
          rename(Value := `Totale Femmine`)) %>% 
  rename(Code := `Codice provincia`, 
         Province = Provincia, 
         Age := `Età`) -> pop20



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


# and aggregate by the selected age groups
pop20 %>% 
  mutate(Age = as.numeric(Age)) %>% # remove NAs since they reflect the Totals
  filter(!is.na(Age)) %>% 
  mutate(
  Age = cut(Age, breaks = c(0, 1, seq(from = 5, to = 100, by = 5), 150), 
            labels = 0:21, 
            include.lowest = TRUE, 
            right = FALSE)
) %>% 
  group_by(Code, Province, Age, sex) %>% 
  summarise(pop = sum(as.numeric(Value))) %>% 
  mutate(year = 2020) -> pop20




# Population for the years 2015-2019 is available here: http://demo.istat.it/ricostruzione/download.php?lingua=ita
# Select the second Province link as you read the page from the top and name it POP2002_2019

pop11_19 <- read_delim("POP2002_2019.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE, 
                              skip = 4)

# We are interested in all nationalities and the years 2015:2019
pop11_19 <- pop11_19[(which(pop11_19$`Territorio/Età` == "Tutte le cittadinanze - Anno: 2011")):
                       (which(pop11_19$`Territorio/Età` == "Cittadinanza italiana - Anno: 2002")), ]


n.dat <- length(unique(as.numeric(pop11_19$`Territorio/Età`)))



# Seperate by sex and bring together
lapply(c("Maschi", "Femmine"), function(Y){
  
  lapply(which(pop11_19$`0` %in% Y), function(X) seq(from = X+1, to  = X + n.dat-1, by = 1)) -> list.sex
  
  pop11_19_sex <- NULL
  
  for(i in 1:length(list.sex)){
    pop11_19_sex_loop <- pop11_19[list.sex[[i]],]
    pop11_19_sex_loop$year <- 2010+i
    pop11_19_sex <- rbind(pop11_19_sex, pop11_19_sex_loop)
  }
  
  return(pop11_19_sex)
}
) -> pop.sex
  
pop.sex[[1]]$sex <- "M"
pop.sex[[2]]$sex <- "F"

pop11_19 <- rbind(pop.sex[[1]], pop.sex[[2]])

# and make long format
pop11_19 <- gather(pop11_19, Age, pop, `0`:`100`)
colnames(pop11_19)[c(1:2)] <- c("Code", "Province")
pop11_19 <- pop11_19[,colnames(pop20)]


# aggregate by age group
pop11_19 %>% 
  mutate(Age = as.numeric(Age)) %>% 
  mutate(
    Age = cut(Age, breaks = c(0, 1, seq(from = 5, to = 100, by = 5), 150), 
              labels = 0:21, 
              include.lowest = TRUE, 
              right = FALSE)
  ) %>% 
  group_by(Code, Province, Age, sex, year) %>% 
  summarise(pop = sum(as.numeric(pop))) -> pop11_19


pop11_20 <- rbind(pop11_19, pop20)








# Fix problems with province names (with respect to the shapefile names)
pop11_20$Province[pop11_20$Province=="Valle d'Aosta/Vallée d'Aoste"] = "Aosta"
pop11_20$Province[pop11_20$Province=="Bolzano/Bozen"] = "Bolzano"
pop11_20$Province[pop11_20$Province=="Forlì-Cesena"] = "Forli'-Cesena"
pop11_20$Province[pop11_20$Province=="Massa-Carrara"] = "Massa Carrara"


head(prov.shp@data)
prov.shp$COD_PROV

# GeograpProvincermation from the shapefile
prov.shp = readOGR("ProvCM01012020_g_WGS84.shp")
geodata = prov.shp@data %>% dplyr::select(COD_RIP,COD_REG,COD_PROV,DEN_UTS,SIGLA)


# Merge pop and geodata to have it in compatible format
pop11_20 = left_join(pop11_20, geodata, by=c("Province"="DEN_UTS"))



# the population file should have the following format, so the population interpolation file
# runs smoothly

# NUTS318CD   ageg  sex year population
# 1         1 less40 male 2015      64769
# 2         1 less40 male 2016      62578
# 3         1 less40 male 2017      68788
# 4         1 less40 male 2018      62038
# 5         1 less40 male 2019      67761
# 6         1 less40 male 2020      60105

colnames(pop11_20)
pop11_20$sex[pop11_20$sex %in% "M"] <- "male"
pop11_20$sex[pop11_20$sex %in% "F"] <- "female"

# Save data
save(pop11_20, file="pop11_20_final.RData")



##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################