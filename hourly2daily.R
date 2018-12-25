
#### function getting the daily data capture

count_na<- function(input_vect){
  
  Cap_h<-array()
  NAN_values = which(is.na(input_vect), arr.ind = FALSE, useNames = TRUE)
  not_NAN=which(!(is.na(input_vect)), arr.ind = FALSE, useNames = TRUE)
  Cap_h[NAN_values]=0
  Cap_h[not_NAN]=1
  david<- sum(Cap_h)/24*100
  return(david)
  
}


#### function getting the mean data by ignoring the NA values


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



#### function changing hourly data to daily data


hour2day <- function(hour2day ){
  #hour2day<-to_walash_ # to be deleted 
  
  # obtaining the details of the data
  
  Site <- unique(hour2day$Site) 
  Pollutant<-unique(hour2day$Pollutant)
  Site_Type<-unique(hour2day$Site_Type)
  Unit<-unique(hour2day$Unit)
  Latitude<-unique(hour2day$Latitude)
  Longitude<-unique(hour2day$Longitude)
  Emirate<-unique(hour2day$Emirate)
  Authority<-unique(hour2day$Authority)
  Total_Polutants<-unique(hour2day$Total_Polutants)

Data_frame_all<-as.data.frame( cbind(Site,Pollutant,Site_Type,Unit,Latitude,Longitude,Emirate,Authority,Total_Polutants))


 # capturing the daily data

  hour2day <- hour2day %>%
    mutate( date_form= format(DateTime, "%Y-%m-%d"))%>%
    mutate(date_re=ymd(date_form))%>%
    select(-date_form)
  
  hour2day_2<- setNames(aggregate(hour2day$Value, by=list(hour2day$date_re), mean_na), c("Date", "Daily_mean"))
  
  # capturing the data capture
  data_capture <- setNames(aggregate(hour2day$Value, by=list(hour2day$date_re), count_na), c("Date", "Data_Capture"))
  
  # replicating the station details for all the days
  
  Data_frame_all<-as.data.frame(lapply(Data_frame_all, rep, nrow(hour2day_2)))
  
  
  # combining all of the values
  
  Final_output<- cbind(Data_frame_all, data_capture$Data_Capture, hour2day_2 ) 
  
  return(Final_output)
  
}
