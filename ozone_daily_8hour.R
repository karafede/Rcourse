
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Rcourse")
source("mean_na.r")

##############
#### NCMS ####
##############

# NCMS_2013 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/hourly_FK_new/hourly_data/test_FK/database_NCMS data 2013_hourly.csv")
# 
# head(NCMS_2013)
# 
# david_resutl<-O3_daily_8hourly(NCMS_2013, 2013 )
# 
# NCMS_2013_8hour<- as.data.frame(david_resutl[1])
# NCMS_2013_daily<- as.data.frame(david_resutl[2])
# 
# write_csv(NCMS_2013_8hour, "8hour_O3/database_NCMS_2013_O3_8hour.csv", na = "NA")
# write_csv(NCMS_2013_daily, "Daily_O3/database_NCMS_2013_O3_daily.csv", na = "NA")
# 
# remove(david_resutl, NCMS_2013)
# 
# 
# NCMS_2014 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/database_NCMS_2014_hourly.csv")
# NCMS_2014 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/hourly_FK_new/hourly_data/test_FK/database_NCMS data 2014_hourly.csv")
# 
# david_resutl<-O3_daily_8hourly(NCMS_2014, 2014 )
# 
# NCMS_2014_8hour<- as.data.frame(david_resutl[1])
# NCMS_2014_daily<- as.data.frame(david_resutl[2])
# 
# write_csv(NCMS_2014_8hour, "8hour_O3/database_NCMS_2014_O3_8hour.csv", na = "NA")
# write_csv(NCMS_2014_daily, "Daily_O3/database_NCMS_2014_O3_daily.csv", na = "NA")
# 
# remove(david_resutl, NCMS_2014)



NCMS_2015 <- read_csv("database_NCMS Data 2015_hourly.csv")

david_resutl <- O3_daily_8hourly(NCMS_2015, 2015 )

NCMS_2015_8hour<- as.data.frame(david_resutl[1])
NCMS_2015_daily <- as.data.frame(david_resutl[2])

NCMS_2015_8hour <- na.omit(NCMS_2015_8hour)
NCMS_2015_daily <- na.omit(NCMS_2015_daily)


# or
# remove lines wtih NA in the Mean_8hour column
NCMS_2015_8hour <- NCMS_2015_8hour[!is.na(NCMS_2015_8hour$Value),]
NCMS_2015_daily <- NCMS_2015_daily[!is.na(NCMS_2015_daily$Mean_8hour),]

write_csv(NCMS_2015_8hour, "8hour_O3/database_NCMS_2015_O3_8hour.csv", na = "NA")
write_csv(NCMS_2015_daily, "Daily_O3/database_NCMS_2015_O3_daily.csv", na = "NA")

remove(david_resutl, NCMS_2015)



# NCMS_2016 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/hourly_FK_new/hourly_data/test_FK/database_NCMS data 2016_hourly.csv")
# 
# david_resutl<-O3_daily_8hourly(NCMS_2016, 2016 )
# 
# NCMS_2016_8hour<- as.data.frame(david_resutl[1])
# NCMS_2016_daily<- as.data.frame(david_resutl[2])
# 
# write_csv(NCMS_2016_8hour, "8hour_O3/database_NCMS_2016_O3_8hour.csv", na = "NA")
# write_csv(NCMS_2016_daily, "Daily_O3/database_NCMS_2016_O3_daily.csv", na = "NA")
# 
# remove(david_resutl, NCMS_2016)




##############
##### DM #####
##############

# DM_2013 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/hourly_FK_new/hourly_data/test_FK/database_DM data 2013_hourly.csv")
# 
# 
# david_resutl<-O3_daily_8hourly(DM_2013, 2013 )
# 
# DM_2013_8hour<- as.data.frame(david_resutl[1])
# DM_2013_daily<- as.data.frame(david_resutl[2])
# 
# write_csv(DM_2013_8hour, "8hour_O3/database_DM_2013_O3_8hour.csv", na = "NA")
# write_csv(DM_2013_daily, "Daily_O3/database_DM_2013_O3_daily.csv", na = "NA")
# 
# remove(david_resutl, DM_2013)
# 
# 
# DM_2014 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/hourly_FK_new/hourly_data/test_FK/database_DM data 2014_hourly.csv")
# 
# 
# david_resutl<-O3_daily_8hourly(DM_2014, 2014 )
# 
# DM_2014_8hour<- as.data.frame(david_resutl[1])
# DM_2014_daily<- as.data.frame(david_resutl[2])
# 
# write_csv(DM_2014_8hour, "8hour_O3/database_DM_2014_O3_8hour.csv", na = "NA")
# write_csv(DM_2014_daily, "Daily_O3/database_DM_2014_O3_daily.csv", na = "NA")
# 
# remove(david_resutl, DM_2014)
# 
# 
# 
# DM_2015 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/hourly_FK_new/hourly_data/test_FK/database_DM data 2015_hourly.csv")
# 
# 
# david_resutl<-O3_daily_8hourly(DM_2015, 2015 )
# 
# DM_2015_8hour<- as.data.frame(david_resutl[1])
# DM_2015_daily<- as.data.frame(david_resutl[2])
# 
# write_csv(DM_2015_8hour, "8hour_O3/database_DM_2015_O3_8hour.csv", na = "NA")
# write_csv(DM_2015_daily, "Daily_O3/database_DM_2015_O3_daily.csv", na = "NA")
# 
# remove(david_resutl, DM_2015)
# 
# 
# DM_2016 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/hourly_FK_new/hourly_data/test_FK/database_DM data 2016_hourly.csv")
# 
# 
# david_resutl<-O3_daily_8hourly(DM_2016, 2016 )
# 
# DM_2016_8hour<- as.data.frame(david_resutl[1])
# DM_2016_daily<- as.data.frame(david_resutl[2])
# 
# write_csv(DM_2016_8hour, "8hour_O3/database_DM_2016_O3_8hour.csv", na = "NA")
# write_csv(DM_2016_daily, "Daily_O3/database_DM_2016_O3_daily.csv", na = "NA")
# 
# remove(david_resutl, DM_2016)



###############
##### EAD #####
###############


# EAD_2013 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/hourly_FK_new/hourly_data/test_FK/database_EAD data 2013_hourly.csv")
# 
# 
# david_resutl<-O3_daily_8hourly(EAD_2013, 2013 )
# 
# EAD_2013_8hour<- as.data.frame(david_resutl[1])
# EAD_2013_daily<- as.data.frame(david_resutl[2])
# 
# write_csv(EAD_2013_8hour, "8hour_O3/database_EAD_2013_O3_8hour.csv", na = "NA")
# write_csv(EAD_2013_daily, "Daily_O3/database_EAD_2013_O3_daily.csv", na = "NA")
# 
# remove(david_resutl, EAD_2013)
# 
# 
# EAD_2014 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/hourly_FK_new/hourly_data/test_FK/database_EAD data 2014_hourly.csv")
# 
# 
# david_resutl<-O3_daily_8hourly(EAD_2014, 2014 )
# 
# EAD_2014_8hour<- as.data.frame(david_resutl[1])
# EAD_2014_daily<- as.data.frame(david_resutl[2])
# 
# write_csv(EAD_2014_8hour, "8hour_O3/database_EAD_2014_O3_8hour.csv", na = "NA")
# write_csv(EAD_2014_daily, "Daily_O3/database_EAD_2014_O3_daily.csv", na = "NA")
# 
# remove(david_resutl, EAD_2014)
# 
# 
# EAD_2015 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/hourly_FK_new/hourly_data/test_FK/database_EAD data 2015_hourly.csv")
# 
# 
# david_resutl<-O3_daily_8hourly(EAD_2015, 2015 )
# 
# EAD_2015_8hour<- as.data.frame(david_resutl[1])
# EAD_2015_daily<- as.data.frame(david_resutl[2])
# 
# write_csv(EAD_2015_8hour, "8hour_O3/database_EAD_2015_O3_8hour.csv", na = "NA")
# write_csv(EAD_2015_daily, "Daily_O3/database_EAD_2015_O3_daily.csv", na = "NA")
# 
# remove(david_resutl, EAD_2015)
# 
# 
# EAD_2016 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/hourly_FK_new/hourly_data/test_FK/database_EAD data 2016_hourly.csv")
# 
# 
# david_resutl<-O3_daily_8hourly(EAD_2016, 2016 )
# 
# EAD_2016_8hour<- as.data.frame(david_resutl[1])
# EAD_2016_daily<- as.data.frame(david_resutl[2])
# 
# write_csv(EAD_2016_8hour, "8hour_O3/database_EAD_2016_O3_8hour.csv", na = "NA")
# write_csv(EAD_2016_daily, "Daily_O3/database_EAD_2016_O3_daily.csv", na = "NA")
# 
# remove(david_resutl, EAD_2016)




