

mean_na <- function(data_input ){
  result_mean<- mean(data_input,na.rm = TRUE )
  return(result_mean)
}
max_na<- function(data_input ){
  result_max<- max(data_input, na.rm = TRUE )
  if (result_max == -Inf | result_max== Inf ){
    result_max <- NA
  }
  return(result_max)
}
  
count_na<- function(input_vect){
  Cap_h<-array()
  NAN_values = which(is.na(input_vect), arr.ind = FALSE, useNames = TRUE)
  not_NAN=which(!(is.na(input_vect)), arr.ind = FALSE, useNames = TRUE)
  Cap_h[NAN_values]=0
  Cap_h[not_NAN]=1
  david<- sum(Cap_h)/24*100
  return(david)
  
}

CO_daily_8hourly <- function(EAD_data, year ){

  # removing NAN
  EAD_data[sapply(EAD_data,is.na)] = NA 

# preparing the data 
  data <- EAD_data %>%
  mutate(date = DateTime,
         year = year(DateTime),
         day= day(DateTime),
         hour =  hour(DateTime + 300),
         # hour=round(hour(DateTime), units="hours")(hour(DateTime)),
         month = month(DateTime)) %>%
  dplyr:: select(date,
                 Site,
                 year,
                 month,
                 day,
                 hour,
                 Pollutant,
                 Value) %>%
  filter(Pollutant == "CO") # select the pollutant 

# 8 Hour average
  
# the 8 hour moving average 
  
  Cap_h<-array()
  year_Start=paste(year,"-1-1 0:00" ,sep = "")
  year_End=paste(year,"-12-31 23:00" ,sep = "")
  
  if ((year %% 4 == 0  | year %% 400 == 0) & !(year %% 100 == 0 & !(year %% 400 ==0 ) )  ){
    check_list<- seq(
      from=as.POSIXct(year_Start, tz="UTC"),
      to=as.POSIXct(year_End, tz="UTC"),
      by="hour") 
  }else{

    check_list<- seq(
      from=as.POSIXct(year_Start, tz="UTC"),
      to=as.POSIXct(year_End, tz="UTC"),
      by="hour") 
  }
  

  check_list<-as.data.frame(check_list)
  check_list <- check_list %>%
  mutate(year = year(check_list),
         month = month(check_list),
         day= day(check_list),
         hour = hour(check_list ))

# select by stations to loop 
  average_CO_all<- data.frame()
  for (j in da<-unique(data$Site)){
    station_CO= filter( data, Site == j )
    NAN_values = which(is.na(station_CO$Value), arr.ind = FALSE, useNames = TRUE)
    not_NAN=which(!(is.na(station_CO$Value)), arr.ind = FALSE, useNames = TRUE)
    Cap_h[NAN_values]=0
    Cap_h[not_NAN]=1
    for (i in 1:nrow(station_CO)){
      if (i < 8){
        xx= station_CO$Value[1:i]*Cap_h[1:i]
        if (sum(Cap_h[1:i])/8 >= 0.75){
          yy=sum(xx)/sum(Cap_h[1:i])
          yy_cap=sum(Cap_h[1:i])/8*100
        }else{
          yy=NA
          yy_cap=sum(Cap_h[1:i])/8*100
        }
      }
      if (i >= 8){
        xx= station_CO$Value[(i-7):i]*Cap_h[(i-7):i]
        if (sum(Cap_h[(i-7):i])/8 >= 0.75){
          yy=sum(xx,na.rm = TRUE)/sum(Cap_h[(i-7):i])
          yy_cap=sum(Cap_h[(i-7):i])/8*100
        }else{
          yy=NA
          yy_cap=sum(Cap_h[(i-7):i])/8*100
        }
      }
      average_CO<- data.frame (Date=check_list[i, 1] ,Site=j, Pollutant="CO", Value=yy, Capture=yy_cap) 
      average_CO_all <- rbind(average_CO_all,average_CO)
      remove(average_CO)
    }
    
  }
  
  
  # the daily average of ozone in all the stations
  
  data_time <- average_CO_all %>%
    select(Date , Site,Pollutant, Value )%>%
    spread(Site, Value)
  
  dawit_da<-data_time %>%
    mutate(date_gr= format(Date, format="%Y-%m-%d")) %>%
    select(-Date, -Pollutant)%>%
    group_by(date_gr) %>%
    summarise_all(funs(mean_na))
  
  
  # the daily capture of the stations
  dawit_da_cap<-data_time %>%
    mutate(date_gr= format(Date, format="%Y-%m-%d")) %>%
    select(-Date, -Pollutant)%>%
    group_by(date_gr) %>%
    summarise_all(funs(count_na))
  
  # the daily maximum 8 hour of the stations
  
  dawit_da_max<-data_time %>%
    mutate(date_gr= format(Date, format="%Y-%m-%d")) %>%
    select(-Date, -Pollutant)%>%
    group_by(date_gr) %>%
    summarise_all(funs(max_na))
  
  # getting the mean daily average according to the EPA statndard i.e. stations must have 75% or more data on daily baisis to estimate the mean 
  
  daily_data<- select(dawit_da,-date_gr)
  daily_capture<-select(dawit_da_cap,-date_gr)
  
  
  for (i in 1:ncol(daily_data)){
    check_T_F<-daily_capture[,i] >= 75
    ind_false <- which(!(check_T_F))
    daily_data[ind_false,i]<- NA
  }
  
  daily_CO_max<-gather( dawit_da_max, "Site" , "MAX_8hour", -date_gr )
  
  daily_data$Date <-dawit_da$date_gr
  
  daily_CO<-gather(daily_data, "Site" , "Mean_8hour" , -Date)
  daily_CO_cap<-gather(daily_capture, "Site" , "Capture" )
  
  daily_CO$MAX_8hour<-daily_CO_max$MAX_8hour
  daily_CO$Capture<-daily_CO_cap$Capture
  
  
  # results of the function CO_daily_8hourly
  result<- list(average_CO_all,daily_CO)
  return(result)

}