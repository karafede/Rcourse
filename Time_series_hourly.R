
# time series with hourly UAE pollution data

library(threadr)
library(dygraphs)
library(tidyr)
library(leaflet)
library(readr)
library(lubridate)
library(ggplot2)

# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data")
# setwd("E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/Interactive_plots_R")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Rcourse")
# setwd("D:/AQ_data/hourly_FK_new/hourly_data/test_FK")

# NCMS <- read_csv("database_NCMS_2016_hourly.csv")
EAD <- read_csv("database_EAD data 2016_hourly.csv")

# replace NaN (not a Number with NA that is a missing value)
EAD[sapply(EAD,is.na)] = NA 

str(EAD)

EAD <- EAD %>%
mutate(date = ymd_hms(DateTime, tz = "UTC"))
  
# shift time back of 4 hours (UAE time)    
EAD <- EAD %>%
    mutate(date = DateTime - 14400)                

 # EAD$date <- (EAD$date) - 4*60*60 #### 3 hours

data_time <- EAD %>%
  filter(Site == "Bain Aljesrain") %>%
  select(date,
         Site,
         Pollutant,
         Value) 

# remove line with date == NA
# data_time <- data_time[!is.na(data_time$date),]

# spread values to have one column for each pollutant
data_time <- data_time %>%
  spread(Pollutant, Value)

############################################################################
## static time series ######################################################
###################################################################################################################
######### plot TIME-SERIES of AQ PM10 data and WRF PM10 data ######################################################
###################################################################################################################


jpeg('time_series_hourly.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

min <- as.POSIXct("2016-03-01 09:00:00") 
max <- as.POSIXct("2016-10-03 22:00:00") 

plot <- ggplot(data_time, aes(date, value)) + 
  theme_bw() +
  geom_line(aes(y = PM2.5, col = "PM2.5"), alpha=1, col="red") +
  geom_line(aes(y = PM10, col = "PM10"), alpha=1, col="blue") +
#  facet_wrap( ~ Site, ncol=4) +
  theme(legend.position="none") + 
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste(PM[10], " (µg/",m^3, ")", " AQ & WRFChem (hourly)"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=14, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=14, colour = "black")) +
  ylim(0, 3000) +
  xlim(min, max)
plot


par(oldpar)
dev.off()








jpeg('time_series_hourly_by_site.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_time <- EAD %>%
  select(date,
         Site,
         Pollutant,
         Value) 

data_time <- data_time %>%
  spread(Pollutant, Value)

min <- as.POSIXct("2016-03-01 09:00:00") 
max <- as.POSIXct("2016-10-03 22:00:00") 


plot <- ggplot(data_time, aes(date, value)) + 
  theme_bw() +
  geom_line(aes(y = PM2.5, col = "PM2.5"), alpha=1, col="red") +
  facet_wrap( ~ Site, ncol=4) +
  theme(legend.position="none") + 
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")", " AQ & WRFChem (hourly)"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=14, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=14, colour = "black")) +
  ylim(0, 3000) +
  xlim(min, max)
plot


par(oldpar)
dev.off()




#############################################################################



####### correlations #############################################################
###################################################################################################################
###################################################################################################################

# check your data  PM10 measurements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
jpeg('boxplots.jpg',
     quality = 100, bg = "white", res = 300, width = 9, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


EAD_PM25 <- EAD %>%
  filter(Pollutant == "PM2.5") 



plot <- ggplot(EAD_PM25, aes(Site, Value)) +
  theme_bw() +
  geom_boxplot() + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=12, colour = "black", face="bold")) +
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")  Measured (hourly)"),size=20)) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=20),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  guides(fill=FALSE)  
plot


par(oldpar)
dev.off()









AQ_WRF_2015_PM10 <- AQ_WRF_2015_PM10 %>%
  filter(Value < 2500) %>%   #check background value (this value has been estimated from the above boxplot)
  filter(Value > 0) %>%
  filter(WRF_CHEM >0)
# filter(WRF_CHEM < 1000)



#### fit function and label for PM AQ and WRF-CHEM data  ########################
#### this funtion FORCE regression to pass through the origin ###################

lm_eqn <- function(df){
  m <- lm(Value ~ -1 + WRF_CHEM, df);
  eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(b = format(coef(m)[1], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}


## this function includes the intercept~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lm_eqn <- function(df){
#   m <- lm(Value ~  WRF_CHEM, df);
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
#                    list(a = format(coef(m)[2], digits = 2),
#                         b = format(coef(m)[1], digits = 2),
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));
# }


################### PM10 versus WRF-chem Dust #############################


# plot with regression line-----

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/PM10_vs_WRF.jpg',    
     quality = 100, bg = "white", res = 200, width = 15, height = 8, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


# define regression equation for each season
eq_PM10 <- ddply(AQ_WRF_2015_PM10, .(Site),lm_eqn)


# ggplot(PM25_AOD, aes(x=Val_AOD, y=Val_PM25, color = season)) +
ggplot(AQ_WRF_2015_PM10, aes(x=WRF_CHEM, y=Value)) +
  theme_bw() +
  # geom_point(size = 2) +
  geom_jitter(colour=alpha("black",0.15) ) +
  facet_wrap( ~ Site, ncol=4) +
  # facet_wrap( ~ day, ncol=2)
  # facet_wrap( ~ Authority, ncol=2)
  theme(strip.text = element_text(size = 10)) + 
  scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
  #  geom_smooth(method="lm") +  # Add linear regression line
  geom_smooth(method = "lm", formula = y ~ -1 + x) +  # force fit through the origin
  ylab(expression(paste(PM[10], " (µg/",m^3, ")", " ", "measurements"))) +
  xlab(expression(paste(PM[10], " (µg/",m^3, ")", " ", "WRF-Chem"))) +
  ylim(c(0, 2500)) + 
  xlim(c(0, 2500)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=8)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=10)) +
  
  geom_text(data = eq_PM10, aes(x = 2000, y = 2000, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 3, color = "black" )



par(oldpar)
dev.off()







#############################################################################
#############################################################################
## dynamic time series ######################################################



# Build timeseries for plots
time_series <- data_frame_to_timeseries(data_time)

# Return
time_series

# make interactive time-series plot
colour_vector <- threadr::ggplot2_colours(45)


plot <- dygraph(time_series$`Lower.Ambient.Temperature`) %>% 
  dyOptions(colors = colour_vector[1]) %>% 
  dySeries(label = "Lower Ambient Temperature") %>% 
  dyAxis("y", label = "Hourly Temp. <sup>o</sup>C") %>% 
  dyRangeSelector()
plot


plot <- dygraph(time_series$PM10) %>% 
  dyOptions(colors = colour_vector[1]) %>% 
  dySeries(label = "PM10") %>% 
  dyAxis("y", label = "Daily PM<sub>10</sub> (&#956;g m<sup>-3</sup>)") %>% 
  dyRangeSelector()
plot

plot <- dygraph(time_series$PM10) %>% 
  dyOptions(colors = colour_vector[1]) %>% 
  dySeries(label = "PM10") %>% 
  dyAxis("y", label = "Daily PM<sub>10</sub> (&#956;g m<sup>-3</sup>)") %>% 
  dyRangeSelector()
plot


plot <- dygraph(time_series$SO2) %>% 
  dyOptions(colors = colour_vector[1]) %>% 
  dySeries(label = "SO2") %>% 
  dyAxis("y", label = "Daily SO<sub>2</sub> (&#956;g m<sup>-3</sup>)") %>% 
  dyRangeSelector()
plot




