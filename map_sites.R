
library(raster)
library(leaflet)
library(htmlwidgets)
library(readr)

#load PM10 stations in the UAE
stations_PM10 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Rcourse/sites_PM10/sites_PM10_DUST_EVENT.csv")
stations_PM10_selected <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Rcourse/sites_PM10/sites_PM10_DUST_EVENT_selected.csv")

# load coordinates of the NCMS monitoring weather stations:
NCMS_STATIONS_COORDS <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Rcourse/weather_sites/stations_clean_FK.csv") 
colnames(NCMS_STATIONS_COORDS) <- c("station", "latitude", "longitude")

NCMS_STATIONS_COORDS_selected <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Rcourse/weather_sites/stations_clean_FK_selected.csv") 
colnames(NCMS_STATIONS_COORDS_selected) <- c("station", "latitude", "longitude")

PM25_tiff <- raster("PM25_MODIS_1km_UAE_242.tif")
plot(PM25_tiff)



map <- leaflet() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addCircleMarkers(data = NCMS_STATIONS_COORDS_selected,
                   lng = ~ longitude, lat = ~ latitude,
                   radius = 9, stroke = TRUE, fillOpacity = 1, popup = ~ station,
                   color = "red", 
                   group = "sites_NCMS_selected") %>%

  
    addCircleMarkers(data = NCMS_STATIONS_COORDS,
                   lng = ~ longitude, lat = ~ latitude,
                   radius = 4, stroke = FALSE, fillOpacity = 0.5, popup = ~ station,
                   color = "blue",
                   group = "sites_NCMS") %>%
  
  # addMarkers(data = NCMS_STATIONS_COORDS_selected , lng = ~ longitude, lat = ~ latitude,
  #            popup = ~ station, group = "sites_NCMS_selected") %>%

  addLayersControl(
    baseGroups = c("Toner Lite", "Road map", "Topographical", "Satellite"),
    overlayGroups = c("sites_NCMS_selected",  "sites_NCMS"),
    options = layersControlOptions(collapsed = TRUE)) %>%
  hideGroup(c("sites_NCMS_selected")) 

map



# save map
saveWidget(map, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Rcourse/weather_sites/met_stations_NCMS.html", selfcontained = FALSE)

popup <- paste0("<strong><i>",
                stations_PM10$Site,
                "</i></strong><br>Hourly PM<sub>10</sub>: <strong> ", round(stations_PM10$Value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")


pal_PM25 <- colorNumeric(c("#9999FF", "#ffd699", "#FFFF00", "#ffbf00", "#ffc700", "#FF0000", "#994c00"),
                         getValues(PM25_tiff),na.color = "transparent")

map <- leaflet() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  
  addCircleMarkers(data = stations_PM10_selected,
                   lng = ~ Longitude, lat = ~ Latitude,
                   radius = 9, stroke = TRUE, fillOpacity = 1, popup = stations_PM10_selected$Site,
                   color = "black", 
                   group = "sites_PM10_selected") %>%
  
  
  addCircleMarkers(data = stations_PM10,
                   lng = ~ Longitude, lat = ~ Latitude,
                   radius = 4, stroke = FALSE, fillOpacity = 0.5, popup = popup,
                   color = "blue",
                   group = "sites_PM10") %>%
  
  
  
  addRasterImage(PM25_tiff, colors = pal_PM25, opacity = 0.4,
                 group = "PM25_tiff") %>%
  
  
  addLegend("bottomright",pal = pal_PM25, values = values(PM25_tiff),
            title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) (1km) <br />: </strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.8) %>%

  
  addLayersControl(
    baseGroups = c("Toner Lite", "Road map", "Topographical", "Satellite"),
    overlayGroups = c("sites_PM10_selected",  "sites_PM10", "PM25_tiff"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("sites_PM10_selected")) 

map

saveWidget(map, paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Rcourse/sites_PM10/Stations_PM10.html"), selfcontained = FALSE)
