

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/")

setwd("E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data")


###########################################################################
###########################################################################


EAD_data_2013 <- read_csv("database_EAD_2013_daily.csv")
EAD_data_2014 <- read_csv("database_EAD_2014_daily.csv")
EAD_data_2015 <- read_csv("database_EAD_2015_daily.csv")
EAD_data_2016 <- read_csv("database_EAD_2016_daily.csv")

DM_data_2013 <- read_csv("database_DM_2013_daily.csv")
DM_data_2014 <- read_csv("database_DM_2014_daily.csv")
DM_data_2015 <- read_csv("database_DM_2015_daily.csv")
DM_data_2016 <- read_csv("database_DM_2016_daily.csv")

NCMS_data_2013 <- read_csv("database_NCMS_2013_daily.csv")
NCMS_data_2014 <- read_csv("database_NCMS_2014_daily.csv")
NCMS_data_2015 <- read_csv("database_NCMS_2015_daily.csv")
NCMS_data_2016 <- read_csv("database_NCMS_2016_daily.csv")

AQ_data <- rbind(EAD_data_2013, EAD_data_2014, EAD_data_2015, EAD_data_2016, 
                 DM_data_2013, DM_data_2014, DM_data_2015, DM_data_2016,
                 NCMS_data_2013, NCMS_data_2014, NCMS_data_2015, NCMS_data_2016)

# replace NaN (not a Number with NA that is a missing value)
AQ_data[sapply(AQ_data,is.na)] = NA 


 # load Ozone data

wd <- getwd()
EAD_O3_2013 <- read_csv(paste0(wd,"/Daily_O3/database_EAD_2013_O3_daily.csv"))
EAD_O3_2014 <- read_csv(paste0(wd,"/Daily_O3/database_EAD_2014_O3_daily.csv"))
EAD_O3_2015 <- read_csv(paste0(wd,"/Daily_O3/database_EAD_2015_O3_daily.csv"))
EAD_O3_2016 <- read_csv(paste0(wd,"/Daily_O3/database_EAD_2016_O3_daily.csv"))

DM_O3_2013 <- read_csv(paste0(wd,"/Daily_O3/database_DM_2013_O3_daily.csv"))
DM_O3_2014 <- read_csv(paste0(wd,"/Daily_O3/database_DM_2014_O3_daily.csv"))
DM_O3_2015 <- read_csv(paste0(wd,"/Daily_O3/database_DM_2015_O3_daily.csv"))
DM_O3_2016 <- read_csv(paste0(wd,"/Daily_O3/database_DM_2016_O3_daily.csv"))

NCMS_O3_2013 <- read_csv(paste0(wd,"/Daily_O3/database_NCMS_2013_O3_daily.csv"))
NCMS_O3_2014 <- read_csv(paste0(wd,"/Daily_O3/database_NCMS_2014_O3_daily.csv"))
NCMS_O3_2015 <- read_csv(paste0(wd,"/Daily_O3/database_NCMS_2015_O3_daily.csv"))
NCMS_O3_2016 <- read_csv(paste0(wd,"/Daily_O3/database_NCMS_2016_O3_daily.csv"))

O3_data <- rbind(EAD_O3_2013, EAD_O3_2014, EAD_O3_2015, EAD_O3_2016,
                 DM_O3_2013, DM_O3_2014, DM_O3_2015, DM_O3_2016,
                 NCMS_O3_2013, NCMS_O3_2014, NCMS_O3_2015, NCMS_O3_2016)


# replace NaN (not a Number with NA that is a missing value)
O3_data[sapply(O3_data,is.na)] = NA 

O3_data$Mean_8hour <- as.numeric(O3_data$Mean_8hour)
O3_data$MAX_8hour <- as.numeric(O3_data$MAX_8hour)


O3_data <- O3_data %>%
  mutate(year = year(Date)) %>%
  dplyr:: select(Date,
                 Site,
                 MAX_8hour,
                 year,
                 Capture)
str(O3_data)


# load CO data

EAD_CO_2013 <- read_csv(paste0(wd,"/Daily_CO/database_EAD_2013_CO_daily.csv"))
EAD_CO_2014 <- read_csv(paste0(wd,"/Daily_CO/database_EAD_2014_CO_daily.csv"))
EAD_CO_2015 <- read_csv(paste0(wd,"/Daily_CO/database_EAD_2015_CO_daily.csv"))
EAD_CO_2016 <- read_csv(paste0(wd,"/Daily_CO/database_EAD_2016_CO_daily.csv"))

DM_CO_2013 <- read_csv(paste0(wd,"/Daily_CO/database_DM_2013_CO_daily.csv"))
DM_CO_2014 <- read_csv(paste0(wd,"/Daily_CO/database_DM_2014_CO_daily.csv"))
DM_CO_2015 <- read_csv(paste0(wd,"/Daily_CO/database_DM_2015_CO_daily.csv"))
DM_CO_2016 <- read_csv(paste0(wd,"/Daily_CO/database_DM_2016_CO_daily.csv"))

NCMS_CO_2013 <- read_csv(paste0(wd,"/Daily_CO/database_NCMS_2013_CO_daily.csv"))
NCMS_CO_2014 <- read_csv(paste0(wd,"/Daily_CO/database_NCMS_2014_CO_daily.csv"))
NCMS_CO_2015 <- read_csv(paste0(wd,"/Daily_CO/database_NCMS_2015_CO_daily.csv"))
NCMS_CO_2016 <- read_csv(paste0(wd,"/Daily_CO/database_NCMS_2016_CO_daily.csv"))

CO_data <- rbind(EAD_CO_2013, EAD_CO_2014, EAD_CO_2015, EAD_CO_2016,
                 DM_CO_2013, DM_CO_2014, DM_CO_2015, DM_CO_2016,
                 NCMS_CO_2013, NCMS_CO_2014, NCMS_CO_2015, NCMS_CO_2016)

# replace NaN (not a Number with NA that is a missing value)
CO_data[sapply(CO_data,is.na)] = NA 

CO_data$Mean_8hour <- as.numeric(CO_data$Mean_8hour)
CO_data$MAX_8hour <- as.numeric(CO_data$MAX_8hour)


CO_data <- CO_data %>%
  mutate(year = year(Date)) %>%
  dplyr:: select(Date,
                 Site,
                 MAX_8hour,
                 year,
                 Capture)

##-------------------------------------------------------------------
# boxplots----#######################################################

## PM10----------------------------------------------------------------


AQ_data_PM10 <- AQ_data %>%
 mutate(date = mdy(date, tz = "UTC"),
        year = year(date)) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value,
                 year,
                 Cap) %>%
  filter(Cap > 75) %>%
  filter(Pollutant == "PM10")


# make a box plot with ggplot----------------------------------------


jpeg('summary_plots/PM10_boxplot.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_data_PM10, aes(Site, Value, fill = Site)) +
  theme_bw() +
 geom_boxplot() + 
#  facet_grid(. ~ year) +
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 200) +
  theme( strip.text = element_text(size = 18)) + 
  xlab("Site") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
  ylab(expression(paste(PM[10], " (µg/",m^3, ")"),size=18)) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
  geom_hline(yintercept=150, col="red", size = 1) +
  ggtitle(expression(paste("Distribution of 24h-averaged"," ", PM[10], " concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) 
  
plot


par(oldpar)
dev.off()

# ### Data Capture PM10 ###########
# 
# jpeg('summary_plots/PM10_capture.jpg',
#      quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
# par(mar=c(4, 10, 9, 2) + 0.3)
# oldpar <- par(las=1)
# 
# 
# plot <- ggplot(AQ_data_PM10, aes(Site, Cap, fill = Site)) +
#   theme_bw() +
#   geom_boxplot() + 
#   #  facet_grid(. ~ year) +
#   facet_grid(year ~ .) +
#   guides(fill=FALSE) +   # no legend
#   ylim(0, 100) +
#   theme( strip.text = element_text(size = 18)) + 
#   xlab("Site") +
#   ylab(expression("Data Capture (%)")) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
#   theme(axis.title.y = element_text(face="bold", colour="black", size=18),
#         axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
#   geom_hline(yintercept=75, col="red", size = 1) +
#   ggtitle(expression(paste("Data Capture"," ","(", PM[10],")"))) + 
#   theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
# plot
# 
# 
# par(oldpar)
# dev.off()


### Annual mean PM10 concentration #################################

jpeg('summary_plots/PM10_annual.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

AQ_data_PM10_AVG <- AQ_data_PM10 %>%
  group_by(year,
           Site) %>%
  filter(Value > 0) %>%
  filter(Value < 200) %>%
  summarise(AVG_Value = mean(Value))

# Annual Mean UAE-----------------------
Annual_Mean_PM10 <- AQ_data_PM10 %>%
  group_by(year) %>%
  filter(Value > 0) %>%
  filter(Value < 200) %>%
  filter(Cap > 75) %>%
  summarise(mean_PM10 = mean(Value))

Annual_Mean_PM10$year <- as.factor(Annual_Mean_PM10$year)
AQ_data_PM10_AVG$year <- as.factor(AQ_data_PM10_AVG$year)

write_csv(Annual_Mean_PM10, "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Annual_Mean_PM10.csv")


plot <- ggplot(AQ_data_PM10_AVG, aes(year, AVG_Value, fill = year)) +
  theme_bw() +
  geom_boxplot() + 
  ylim(0, 150) +
  guides(fill=FALSE) +   # no legend
  ylab(expression(paste(PM[10], " (µg/",m^3, ")"),size=18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=24, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=24),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=24, colour = "black")) +
  geom_hline(yintercept=40, col="red", size = 1) +
  geom_hline(yintercept=150, col="blue", size = 1) +
  ggtitle(expression(paste("Annual distribution of daily"," ", PM[10]," concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 25, hjust = 0.5)) +
  
  geom_text(aes(x = 1 , y = 35, label = "EU annual limit value"), size = 7) +
  geom_text(aes(x = 1 , y = 145, label = "UAE 24h limit value"), size = 7) +
  
  geom_point(data = Annual_Mean_PM10, aes(year,mean_PM10), 
             color='black', size = 8, shape=18)
  
plot


par(oldpar)
dev.off()


######################################################################
######################################################################

## PM2.5----------------------------------------------------------------


AQ_data_PM25 <- AQ_data %>%
  mutate(date = mdy(date, tz = "UTC"),
         year = year(date)) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value,
                 year,
                 Cap) %>%
  filter(Cap > 75) %>%
  filter(Pollutant == "PM2.5")


jpeg('summary_plots/PM2.5_boxplot.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_data_PM25, aes(Site, Value, fill = Site)) +
  theme_bw() +
  geom_boxplot() + 
  #  facet_grid(. ~ year) +
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 150) +
  xlab("Site") +
  theme( strip.text = element_text(size = 18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
  geom_hline(yintercept=35, col="red", size = 1) +
  ggtitle(expression(paste("Distribution of 24h-averaged"," ", PM[2.5], " concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
plot


par(oldpar)
dev.off()


# ### Data Capture PM2.5 ###########
# 
# jpeg('summary_plots/PM25_capture.jpg',
#      quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
# par(mar=c(4, 10, 9, 2) + 0.3)
# oldpar <- par(las=1)
# 
# 
# plot <- ggplot(AQ_data_PM25, aes(Site, Cap, fill = Site)) +
#   theme_bw() +
#   geom_boxplot() + 
#   #  facet_grid(. ~ year) +
#   facet_grid(year ~ .) +
#   guides(fill=FALSE) +   # no legend
#   ylim(0, 100) +
#   theme( strip.text = element_text(size = 18)) + 
#   xlab("Site") +
#   ylab(expression("Data Capture (%)")) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
#   theme(axis.title.y = element_text(face="bold", colour="black", size=18),
#         axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
#   geom_hline(yintercept=75, col="red", size = 1) +
#   ggtitle(expression(paste("Data Capture"," ","(", PM[2.5],")"))) + 
#   theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
# plot
# 
# 
# par(oldpar)
# dev.off()



### Annual mean PM2.5 concentration #################################

jpeg('summary_plots/PM2.5_annual.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

AQ_data_PM25_AVG <- AQ_data_PM25 %>%
  group_by(year,
           Site) %>%
  filter(Value > 0) %>%
  filter(Value < 200) %>%
  filter(Cap > 75) %>%
  summarise(AVG_Value = mean(Value))

# Annual Mean UAE-----------------------
Annual_Mean_PM25 <- read_csv("E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Annual_means_Quarters_PM2.5_new.csv")


Annual_Mean_PM25 <- Annual_Mean_PM25 %>%
  group_by(year) %>%
    summarise(mean_PM25 = mean(annual_AVG, na.rm=TRUE))

write_csv(Annual_Mean_PM25, "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Annual_Mean_PM25.csv")


# Annual Mean UAE-----------------------
# Annual_Mean_PM25 <- AQ_data_PM25 %>%
#   group_by(year) %>%
#   filter(Value > 0) %>%
#   filter(Value < 200) %>%
#   summarise(mean_PM25 = mean(Value))

AQ_data_PM25_AVG$year <- as.factor(AQ_data_PM25_AVG$year)
Annual_Mean_PM25$year <- as.factor(Annual_Mean_PM25$year)


plot <- ggplot(AQ_data_PM25_AVG, aes(year, AVG_Value, fill = year)) +
  theme_bw() +
  geom_boxplot() + 
  ylim(0, 75) +
  guides(fill=FALSE) +   # no legend
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")"),size=18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=24, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=24),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=24, colour = "black")) +
  geom_hline(yintercept=15, col="blue", size = 1) +
  geom_hline(yintercept=35, col="red", size = 1) +
  ggtitle(expression(paste("Annual distribution of daily"," ", PM[2.5]," concentration"))) +  
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 25, hjust = 0.5)) +
  
  # geom_text(aes(x = 0.75 , y = 23, label = "EU limit value"), size = 7) +
  # geom_hline(yintercept=25, col="red", size = 1) +
  
  geom_text(aes(x = 0.9 , y = 38, label = "EPA 24h limit value"), size = 7) +
  geom_text(aes(x = 0.95 , y = 18, label = "EPA annual limit value"), size = 7) +

  
  geom_point(data = Annual_Mean_PM25, aes(year,mean_PM25), 
             color='black', size = 8, shape=18)

plot


par(oldpar)
dev.off()




######################################################################
######################################################################

# SO2 and NO2 (1-hr)---------------------------------------------------------------------
# for EAD use filtered data (4 boxplot)
dir_SO2_NO2 <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/filtered_4_box" 
dir_SO2_NO2 <- "E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/filtered_4_box"

EAD_SO2_NO2_2013 <- read_csv(paste0(dir_SO2_NO2, "/","database_EAD_ 2013 _hourly_filtered.csv"))
EAD_SO2_NO2_2014 <- read_csv(paste0(dir_SO2_NO2, "/","database_EAD_ 2014 _hourly_filtered.csv"))
EAD_SO2_NO2_2015 <- read_csv(paste0(dir_SO2_NO2, "/","database_EAD_ 2015 _hourly_filtered.csv"))
# EAD_SO2_NO2_2015$DateTime <- EAD_SO2_NO2_2015$DateTime +3
EAD_SO2_NO2_2016 <- read_csv(paste0(dir_SO2_NO2, "/","database_EAD_ 2016 _hourly_filtered.csv"))
# EAD_SO2_NO2_2016$DateTime <- EAD_SO2_NO2_2016$DateTime +3

DM_SO2_NO2_2013 <- read_csv(paste0(dir_SO2_NO2, "/","database_DM_ 2013 _hourly_filtered.csv"))
# DM_SO2_NO2_2013$DateTime <- DM_SO2_NO2_2013$DateTime +3
DM_SO2_NO2_2014 <- read_csv(paste0(dir_SO2_NO2, "/","database_DM_ 2014 _hourly_filtered.csv"))
#DM_SO2_NO2_2014$DateTime <- DM_SO2_NO2_2014$DateTime +3
DM_SO2_NO2_2015 <- read_csv(paste0(dir_SO2_NO2, "/","database_DM_ 2015 _hourly_filtered.csv"))
# DM_SO2_NO2_2015$DateTime <- DM_SO2_NO2_2015$DateTime +3
DM_SO2_NO2_2016 <- read_csv(paste0(dir_SO2_NO2, "/","database_DM_ 2016 _hourly_filtered.csv"))
#DM_SO2_NO2_2016$DateTime <- DM_SO2_NO2_2016$DateTime +3

NCMS_SO2_NO2_2013 <- read_csv(paste0(dir_SO2_NO2, "/","database_NCMS_ 2013 _hourly_filtered.csv"))
NCMS_SO2_NO2_2014 <- read_csv(paste0(dir_SO2_NO2, "/","database_NCMS_ 2014 _hourly_filtered.csv"))
NCMS_SO2_NO2_2015 <- read_csv(paste0(dir_SO2_NO2, "/","database_NCMS_ 2015 _hourly_filtered.csv"))
NCMS_SO2_NO2_2016 <- read_csv(paste0(dir_SO2_NO2, "/","database_NCMS_ 2016 _hourly_filtered.csv"))


SO2_NO2_all <- rbind(EAD_SO2_NO2_2013, EAD_SO2_NO2_2014, EAD_SO2_NO2_2015, EAD_SO2_NO2_2016,
                     DM_SO2_NO2_2013, DM_SO2_NO2_2014, DM_SO2_NO2_2015, DM_SO2_NO2_2016,
                     NCMS_SO2_NO2_2013, NCMS_SO2_NO2_2014, NCMS_SO2_NO2_2015, NCMS_SO2_NO2_2016)

SO2_NO2_all <- SO2_NO2_all %>%
  select(DateTime,
         Site,
         Pollutant,
         Site_Type,
         Latitude,
         Longitude,
         Value) 


## NO2----------------------------------------------------------------



AQ_data_NO2 <- SO2_NO2_all %>%
  mutate(date = ymd_hms(DateTime, tz = "UTC"),
         year = year(date)) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value,
                 year) %>%
  filter(Pollutant == "NO2")



jpeg('summary_plots/NO2_boxplot.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_data_NO2, aes(Site, Value, fill = Site)) +
  theme_bw() +
  geom_boxplot() + 
  #  facet_grid(. ~ year) +
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 200) +
  xlab("Site") +
  theme( strip.text = element_text(size = 18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
  ylab(expression(paste(NO[2], " (µg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
#  geom_hline(yintercept= 400, col="red", size = 1) +
  ggtitle(expression(paste("Distribution of hourly"," ", NO[2], " concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
plot


par(oldpar)
dev.off()



### Data Capture NO2 ###########

# jpeg('summary_plots/NO2_capture.jpg',
#      quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
# par(mar=c(4, 10, 9, 2) + 0.3)
# oldpar <- par(las=1)
# 
# 
# plot <- ggplot(AQ_data_NO2, aes(Site, Cap, fill = Site)) +
#   theme_bw() +
#   geom_boxplot() + 
#   #  facet_grid(. ~ year) +
#   facet_grid(year ~ .) +
#   guides(fill=FALSE) +   # no legend
#   ylim(0, 100) +
#   theme( strip.text = element_text(size = 18)) + 
#   xlab("Site") +
#   ylab(expression("Data Capture (%)")) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
#   theme(axis.title.y = element_text(face="bold", colour="black", size=18),
#         axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
#   geom_hline(yintercept=75, col="red", size = 1) +
#   ggtitle(expression(paste("Data Capture"," ","(", NO[2],")"))) + 
#   theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
# plot
# 
# 
# par(oldpar)
# dev.off()


### Annual mean NO2 concentration #################################

jpeg('summary_plots/NO2_annual.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

AQ_data_NO2_AVG <- AQ_data_NO2 %>%
  group_by(year,
           Site) %>%
   filter(Value > 0) %>%
  # filter(Value < 200) %>%
  summarise(AVG_Value = mean(Value))


# Annual Mean UAE-----------------------
Annual_Mean_NO2 <- AQ_data_NO2 %>%
  group_by(year) %>%
  filter(Value > 0) %>%
  summarise(mean_NO2 = mean(Value))

write_csv(Annual_Mean_NO2, "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Annual_Mean_NO2.csv")


Annual_Mean_NO2$year <- as.factor(Annual_Mean_NO2$year)
AQ_data_NO2_AVG$year <- as.factor(AQ_data_NO2_AVG$year)


plot <- ggplot(AQ_data_NO2_AVG, aes(year, AVG_Value, fill = year)) +
  theme_bw() +
  geom_boxplot() + 
  ylim(0, 80) +
  guides(fill=FALSE) +   # no legend
  ylab(expression(paste(NO[2], " (µg/",m^3, ")"),size=18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=24, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=24),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=24, colour = "black")) +
  geom_hline(yintercept=40, col="red", size = 1) +
  ggtitle(expression(paste("Annual distribution of hourly"," ", NO[2]," concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 25, hjust = 0.5)) +
  
  geom_text(aes(x = 0.75 , y = 44, label = "EU limit value"), size = 7) +
  
  geom_point(data = Annual_Mean_NO2, aes(year,mean_NO2), 
             color='black', size = 8, shape=18)
plot


par(oldpar)
dev.off()



######################################################################
######################################################################

## SO2----------------------------------------------------------------


AQ_data_SO2 <- SO2_NO2_all %>%
  mutate(date = ymd_hms(DateTime, tz = "UTC"),
         year = year(date)) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value,
                 year) %>%
  filter(Pollutant == "SO2")




jpeg('summary_plots/SO2_boxplot.jpg',
     quality = 100, bg = "white", res = 600, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_data_SO2, aes(Site, Value, fill = Site)) +
  theme_bw() +
  geom_boxplot() + 
  #  facet_grid(. ~ year) +
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 200) +
  xlab("Site") +
  theme( strip.text = element_text(size = 18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
  ylab(expression(paste(SO[2], " (µg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
#  geom_hline(yintercept=197, col="red", size = 1) +
  ggtitle(expression(paste("Distribution of hourly"," ", SO[2], " concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
  

plot


par(oldpar)
dev.off()





### Data Capture SO2 ###########

# jpeg('summary_plots/SO2_capture.jpg',
#      quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
# par(mar=c(4, 10, 9, 2) + 0.3)
# oldpar <- par(las=1)
# 
# 
# plot <- ggplot(AQ_data_SO2, aes(Site, Cap, fill = Site)) +
#   theme_bw() +
#   geom_boxplot() + 
#   #  facet_grid(. ~ year) +
#   facet_grid(year ~ .) +
#   guides(fill=FALSE) +   # no legend
#   ylim(0, 100) +
#   theme( strip.text = element_text(size = 18)) + 
#   xlab("Site") +
#   ylab(expression("Data Capture (%)")) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
#   theme(axis.title.y = element_text(face="bold", colour="black", size=18),
#         axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
#   geom_hline(yintercept=75, col="red", size = 1) +
#   ggtitle(expression(paste("Data Capture"," ","(", SO[2],")"))) + 
#   theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
# plot
# 
# 
# par(oldpar)
# dev.off()
# 


### Annual mean SO2 concentration #################################

jpeg('summary_plots/SO2_annual.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


AQ_data_SO2 <- AQ_data %>%
  mutate(date = mdy(date, tz = "UTC"),
         year = year(date)) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value,
                 year,
                 Cap) %>%
  filter(Pollutant == "SO2")


AQ_data_SO2_AVG <- AQ_data_SO2 %>%
  group_by(year,
           Site) %>%
  filter(Value > 0) %>%
  # filter(Value < 200) %>%
  summarise(AVG_Value = mean(Value))

# Annual Mean UAE-----------------------
Annual_Mean_SO2 <- AQ_data_SO2 %>%
  group_by(year) %>%
  filter(Value > 0) %>%
  summarise(mean_SO2 = mean(Value))

Annual_Mean_SO2$year <- as.factor(Annual_Mean_SO2$year)
AQ_data_SO2_AVG$year <- as.factor(AQ_data_SO2_AVG$year)



write_csv(Annual_Mean_SO2, "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Annual_Mean_SO2.csv")



plot <- ggplot(AQ_data_SO2_AVG, aes(year, AVG_Value, fill = year)) +
  theme_bw() +
  geom_boxplot() + 
  ylim(0, 20) +
  guides(fill=FALSE) +   # no legend
  ylab(expression(paste(SO[2], " (µg/",m^3, ")"),size=18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=24, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=24),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=24, colour = "black")) +
  geom_hline(yintercept=125, col="red", size = 1) +
  ggtitle(expression(paste("Annual distribution of daily"," ", SO[2]," concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 25, hjust = 0.5)) +
  
  #  geom_text(aes(x = 0.75 , y = 37, label = "EU limit value"), size = 7) +
  
  geom_point(data = Annual_Mean_SO2, aes(year,mean_SO2), 
             color='black', size = 8, shape=18)

plot


par(oldpar)
dev.off()




######################################################################
######################################################################

## CO----------------------------------------------------------------

jpeg('summary_plots/CO_boxplot.jpg',
     quality = 100, bg = "white", res = 600, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(CO_data, aes(Site, MAX_8hour, fill = Site)) +
  theme_bw() +
  geom_boxplot() + 
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 3) +
  xlab("Site") +
  theme( strip.text = element_text(size = 18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
  ylab(expression(paste("CO", " (mg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
  geom_hline(yintercept=125, col="red", size = 1) +
  ggtitle("Distribution of Daily Maximum 8-hour mean CO concentration") +  
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
plot


par(oldpar)
dev.off()



### Data Capture CO ###########

# jpeg('summary_plots/CO_capture.jpg',
#      quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
# par(mar=c(4, 10, 9, 2) + 0.3)
# oldpar <- par(las=1)
# 
# 
# plot <- ggplot(CO_data, aes(Site, Capture, fill = Site)) +
#   theme_bw() +
#   geom_boxplot() + 
#   facet_grid(year ~ .) +
#   guides(fill=FALSE) +   # no legend
#   ylim(0, 100) +
#   theme( strip.text = element_text(size = 18)) + 
#   xlab("Site") +
#   ylab(expression("Data Capture (%)")) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
#   theme(axis.title.y = element_text(face="bold", colour="black", size=18),
#         axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
#   geom_hline(yintercept=75, col="red", size = 1) +
#   ggtitle("Data Capture CO") + 
#   theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
# plot
# 
# 
# par(oldpar)
# dev.off()
# 


### Annual mean CO concentration #################################

jpeg('summary_plots/CO_annual.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

AQ_data_CO_AVG <- CO_data %>%
  group_by(year,
           Site) %>%
  filter(MAX_8hour > 0) %>%
   filter(MAX_8hour < 100) %>%
  summarise(AVG_Value = mean(MAX_8hour))


# Annual Mean UAE-----------------------
Annual_Mean_CO <- CO_data %>%
  group_by(year) %>%
  filter(MAX_8hour > 0) %>%
  filter(MAX_8hour < 10) %>%
  summarise(mean_CO = mean(MAX_8hour))

write_csv(Annual_Mean_CO, "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Annual_Mean_CO_Max_8h.csv")


Annual_Mean_CO$year <- as.factor(Annual_Mean_CO$year)
AQ_data_CO_AVG$year <- as.factor(AQ_data_CO_AVG$year)


plot <- ggplot(AQ_data_CO_AVG, aes(year, AVG_Value, fill = year)) +
  theme_bw() +
  geom_boxplot() + 
  ylim(0, 2.5) +
  guides(fill=FALSE) +   # no legend
  ylab(expression(paste("CO", " (mg/",m^3, ")"),size=18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=24, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=24),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=24, colour = "black")) +
 # geom_hline(yintercept=125, col="red", size = 1) +
  ggtitle("Annual distribution of maximum 8h-mean CO concentration") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 25, hjust = 0.5)) +
  
  geom_point(data = Annual_Mean_CO, aes(year,mean_CO), 
             color='black', size = 8, shape=18)

plot


par(oldpar)
dev.off()



############################################################################
############################################################################


# OZONE--------------------------------------------------------------------

jpeg('summary_plots/O3_boxplot.jpg',
     quality = 100, bg = "white", res = 600, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

# load Valid Daily Max according to US-EPA regulations
Daily_Max_O3 <- read_csv("E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Daily_O3/MAX_4_values_o3_transposed.csv")
Daily_Max_O3$year <- as.factor(Daily_Max_O3$year)
Daily_Max_O3$sorted_max_by_year <- Daily_Max_O3$sorted_max_by_year *1960



plot <- ggplot(O3_data, aes(Site, MAX_8hour, fill = Site)) +
  theme_bw() +
  geom_boxplot() + 
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 200) +
  xlab("Site") +
  theme( strip.text = element_text(size = 18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
  ylab(expression(paste(O[3], " (µg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
  geom_hline(yintercept=120, col="red", size = 1) +
  ggtitle(expression(paste("Distribution of Daily Maximum 8-hour mean"," ", O[3], " concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
  
  
plot


par(oldpar)
dev.off()

#################################################
# sorted highest daily max 8-hour concentration ########

jpeg('summary_plots/O3_highest_Daily_Max_boxplot.jpg',
     quality = 100, bg = "white", res = 600, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

# load Valid Daily Max according to US-EPA regulations
Daily_Max_O3 <- read_csv("E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Daily_O3/MAX_4_values_o3_transposed.csv")
Daily_Max_O3$year <- as.factor(Daily_Max_O3$year)
Daily_Max_O3$sorted_max_by_year <- Daily_Max_O3$sorted_max_by_year *1960

EPA_4th_highest_daily <- 1960 * 0.08


plot <- ggplot(Daily_Max_O3, aes(Site, sorted_max_by_year, fill = Site)) +
  theme_bw() +
  geom_point(color="blue", size = 6, shape=17) +
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 300) +
  xlab("Site") +
  theme( strip.text = element_text(size = 18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
  ylab(expression(paste(O[3], " (µg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
  geom_hline(yintercept=156.8, col="red", size = 1) +
  ggtitle(expression(paste("4th highest Daily Maximum 8-hour average"," ", O[3], " concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) 
  
  
  plot


par(oldpar)
dev.off()




### Data Capture O3 ###########

# jpeg('summary_plots/O3_capture.jpg',
#      quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
# par(mar=c(4, 10, 9, 2) + 0.3)
# oldpar <- par(las=1)
# 
# 
# plot <- ggplot(O3_data, aes(Site, Capture, fill = Site)) +
#   theme_bw() +
#   geom_boxplot() + 
#   facet_grid(year ~ .) +
#   guides(fill=FALSE) +   # no legend
#   ylim(0, 100) +
#   theme( strip.text = element_text(size = 18)) + 
#   xlab("Site") +
#   ylab(expression("Data Capture (%)")) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
#   theme(axis.title.y = element_text(face="bold", colour="black", size=18),
#         axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
#   geom_hline(yintercept=75, col="red", size = 1) +
#   ggtitle(expression(paste("Data Capture"," ","(", O[3],")"))) + 
#   theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
# plot
# 
# 
# par(oldpar)
# dev.off()



### Annual mean O3 concentration #################################

jpeg('summary_plots/O3_annual.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


AQ_data_O3_AVG <- O3_data %>%
  group_by(year,
           Site) %>%
  filter(MAX_8hour > 0) %>%
  summarise(AVG_Value = mean(MAX_8hour))


# load Valid Daily Max according to US-EPA regulations
Daily_Max_O3 <- read_csv("E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Daily_O3/MAX_4_values_o3_transposed.csv")

Daily_Max_O3 <- Daily_Max_O3 %>%
  group_by(year) %>%
  summarise(mean_O3 = mean(sorted_max_by_year, na.rm=TRUE))
Daily_Max_O3$mean_O3 <- Daily_Max_O3$mean_O3 *1960

write_csv(Daily_Max_O3, "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Daily_Max_O3.csv")


Daily_Max_O3$year <- as.factor(Daily_Max_O3$year)
AQ_data_O3_AVG$year <- as.factor(AQ_data_O3_AVG$year)



plot <- ggplot(AQ_data_O3_AVG, aes(year, AVG_Value, fill = year)) +
  theme_bw() +
  geom_boxplot() + 
  ylim(0, 200) +
  guides(fill=FALSE) +   # no legend
  ylab(expression(paste(O[3], " (µg/",m^3, ")"),size=18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=24, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=24),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=24, colour = "black")) +
#  geom_hline(yintercept=156.8, col="blue", size = 1,linetype="dashed") +
  geom_hline(yintercept=120, col="red", size = 1) +
  ggtitle(expression(paste("Annual distribution of maximum 8h-mean"," ", O[3]," concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 25, hjust = 0.5)) +
  
  geom_text(aes(x = 1.1 , y = 127, label = "UAE 8h-averaged limit value"), size = 7) +
  
  geom_point(data = Daily_Max_O3, aes(year,mean_O3), 
             color='blue', size = 8, shape=18)

plot


par(oldpar)
dev.off()



######################################################################
######################################################################




######################################################################
######################################################################
######################################################################
######################################################################



######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################



NCMS_data_percentiles <- NCMS_data %>%
  dplyr::group_by(Pollutant) %>%
  summarise("98 percentile" = quantile(Value, c(0.98),  na.rm = TRUE),
            "97 percentile" = quantile(Value, c(0.97),  na.rm = TRUE),
            "96 percentile" = quantile(Value, c(0.96),  na.rm = TRUE),
            "95 percentile" = quantile(Value, c(0.95),  na.rm = TRUE))

#####################################################################
### counting number of observations

EAD_data <- read_csv("database_EAD_2015_daily.csv")

# replace NaN (not a Number with NA that is a missing value)
EAD_data[sapply(EAD_data,is.na)] = NA 
# EAD_data <- na.omit(EAD_data)

EAD_data <- EAD_data %>%
  mutate(date = mdy(date, tz = "UTC")) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value) %>%
  filter(Pollutant == "PM2.5")
 
EAD_data <- EAD_data %>%
  dplyr::group_by(Site) %>%
  summarise("N_above_35ug" = sum(Value > 150, na.rm = TRUE),
            "min" = min(Value, na.rm = TRUE),
            "max" = min(Value, na.rm = TRUE))

# round the values to the nearest integer
EAD_data$min <- round(EAD_data$min, digits = 0)
EAD_data$max <- round(EAD_data$max, digits = 0)

# AAA <- EAD_data$Value > 40
# sum(AAA == TRUE)





##################################################################################
##################################################################################

####################################################################
###########---------------------------------------------------------
## calculation of the annual mean using the EPA protocol


EAD_data <- read_csv("database_EAD_2016_daily.csv")

# get the name of the sites
names <- EAD_data %>% 
  select(Site,
         Value) %>%
  group_by(Site) %>%
  summarise(mean = mean(Value))

list_names <- as.list(names$Site)

# replace NaN (not a Number with NA that is a missing value)
EAD_data[sapply(EAD_data,is.na)] = NA 
# EAD_data <- na.omit(EAD_data)

# percentile_EAD_data <- EAD_data %>%
#   dplyr::group_by(Pollutant,
#                   Site) %>%
#   summarise("98 percentile" = quantile(Value, c(0.98),  na.rm = TRUE),
#             "97 percentile" = quantile(Value, c(0.97),  na.rm = TRUE),
#             "96 percentile" = quantile(Value, c(0.96),  na.rm = TRUE),
#             "95 percentile" = quantile(Value, c(0.95),  na.rm = TRUE))


###############################################################################
# function to all quarterly averages------------------------------------------

quarterly_averages <- function (data, site) {
  
  data <- data %>%
    mutate(date = mdy(date, tz = "UTC"),
           year = year(date),
           month = month(date)) %>%
    dplyr:: select(date,
                   year,
                   month,
                   Pollutant,
                   Value,
                   Cap,
                   Site) %>%
    filter(Pollutant == "PM2.5"  & Site == site)
  
  ## get the months of observations
  
  data$month <- factor(format(data$date, format = "%b"), levels = month.abb)
  
  
  ## Format the quarters
  data$quarter <- character(length = nrow(data))
  data$quarter[data$month %in% month.abb[c(1:3)]] <- "Q1"
  data$quarter[data$month %in% month.abb[c(4:6)]] <- "Q2"
  data$quarter[data$month %in% month.abb[c(7:9)]] <- "Q3"
  data$quarter[data$month %in% month.abb[c(10:12)]] <- "Q4"
  data$quarter <- factor(data$quarter, levels = c("Q1","Q2","Q3","Q4"))
  
  ## year variable
  data$year <- factor(format(data$date, format = "%Y"))
  
  # make averages by quarters------------------------------------------
  AVG_quarter <- data %>%
    group_by(Site,
             quarter,
             year) %>%
    summarise(mean = mean(Value, na.rm = TRUE))
  
  # return
  AVG_quarter <- as.data.frame(AVG_quarter)
  AVG_quarter
  
}


# loop all the stations and concatenate all the data
All_quarters <- data.frame()
for (i in 1:length(list_names)) {
  All_AVG <- quarterly_averages(EAD_data, unlist(list_names[6]))
  All_AVG <- quarterly_averages(EAD_data, unlist(list_names[i]))
  All_quarters <- rbind(All_quarters, All_AVG)
}



# spread data
All_quarters <- All_quarters %>%
  spread(quarter, mean)


All_quarters$annual_AVG <- rowMeans(All_quarters[ ,2:5])


# end












###################################################################################
###################################################################################
# old and alternative stuff #######################################################

###################################################################################
# function to make and aggregate quarterly averages--------------------------------

annual_averages <- function (data, site) {
  
  data <- data %>%
    mutate(date = mdy(date, tz = "UTC"),
           year = year(date),
           month = month(date)) %>%
    dplyr:: select(date,
                   year,
                   month,
                   Pollutant,
                   Value,
                   Cap,
                   Site) %>%
    filter(Pollutant == "PM2.5"  & Site == site)
  
  ## get the months of observations
  
  data$month <- factor(format(data$date, format = "%b"), levels = month.abb)
  
  
  ## Format the quarters
  data$quarter <- character(length = nrow(data))
  data$quarter[data$month %in% month.abb[c(1:3)]] <- "Q1"
  data$quarter[data$month %in% month.abb[c(4:6)]] <- "Q2"
  data$quarter[data$month %in% month.abb[c(7:9)]] <- "Q3"
  data$quarter[data$month %in% month.abb[c(10:12)]] <- "Q4"
  data$quarter <- factor(data$quarter, levels = c("Q1","Q2","Q3","Q4"))
  
  ## year variable
  data$year <- factor(format(data$date, format = "%Y"))
  
  # make averages by quarters------------------------------------------
  AVG_quarter <- data %>%
    group_by(Site,
             quarter) %>%
    summarise(mean = mean(Value, na.rm = TRUE))
  
  
  ## and aggregate for each quarter------------------------------------
  
  AVG_QUARTERS <- with(data[,], aggregate(Value, list(quarter = quarter,
                                                      year = year), FUN = mean, na.rm = TRUE))
  
  
  
  names(AVG_QUARTERS)[names(AVG_QUARTERS) == 'x'] <- 'Quarters'
  
  
  # write.csv(AVG_QUARTERS, paste(site,"_quarterly_AVG.csv", sep = ""), row.names=FALSE)
  Annual_mean <- mean(AVG_QUARTERS$Quarters)
  Annual_Average <- data.frame(c("Site", "year", "Pollutant", "Annual Mean"),
                               c(site, 2015, "PM2.5",Annual_mean))
  
  Annual_Average <- as.data.frame (t(Annual_Average))
  colnames(Annual_Average) <- as.character(unlist(Annual_Average[1,]))
  Annual_Average = Annual_Average[-1, ]
  row.names(Annual_Average) <- NULL
  
  # return dataframe
  Annual_Average
  
}


# loop all the stations and concatenate all the data
All_means <- data.frame()
for (i in 1:length(list_names)) {
  # summary_quarter <- quarterly_averages(EAD_data, "Al Ain Islamic Ins")
  summary_quarter <- annual_averages(EAD_data, unlist(list_names[i]))
  All_means <- rbind(All_means, summary_quarter)
}


#######################################################################################
#######################################################################################


