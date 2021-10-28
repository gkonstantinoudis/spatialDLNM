


# Created 08.10.2021

# Clean and download national holidays


#---------------------------------------------------------------------------------


setwd("E:/Postdoc Imperial/Projects/Spatial DLNMs/data/")

installpack <- FALSE


if(installpack){
  install.packages(c("timeDate", "lubridate", "dplyr"))
}



library(timeDate)
library(lubridate)
library(dplyr)

select_years <- 2010:2020

holidays_vec = sort(c(
  ymd(NewYearsDay(select_years)),
  ymd(ITEpiphany(select_years)),
  ymd(Easter(select_years)),
  ymd(ITLiberationDay(select_years)),
  ymd(LaborDay(select_years)),
  seq(ymd('2010-06-02'), ymd('2020-06-02'), by='years'),
  ymd(ITAssumptionOfVirginMary(select_years)),
  ymd(ITAllSaints(select_years)),
  ymd(ITImmaculateConception(select_years))
)
)

holiday_df = data.frame(Data = holidays_vec)

saveRDS(holiday_df, file="holiday_df")



##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################