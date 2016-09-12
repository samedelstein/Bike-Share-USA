#load libraries
library(jsonlite)
library(leaflet)

#Bike share system station information
chicagourl <- "http://feeds.divvybikes.com/stations/stations.json"
chicagoStations <- fromJSON(chicagourl)
chicagoStations <- chicagoStations$stationBeanList

nycurl <- "https://gbfs.citibikenyc.com/gbfs/en/station_information.json"
nycStations <- fromJSON(nycurl)
nycStations <- nycStations$data$stations

bayareaurl <- "http://feeds.bayareabikeshare.com/stations/stations.json"
bayareaStations <- fromJSON(bayareaurl)
bayareaStations <- bayareaStations$stationBeanList

minnurl <- "http://api-core.niceridemn.org/gbfs/en/station_information.json"
minnStations <- fromJSON(minnurl)
minnStations <- minnStations$data$stations

#build map
head(chicagoStations)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(data = chicagoStations,
                   ~longitude,
                   ~latitude,
                   popup = chicagoStations$stationName,
                   radius = 1) %>%
  addCircleMarkers(data = nycStations,
                   ~lon,
                   ~lat,
                   popup = nycStations$name,
                   radius=1)%>%
  addCircleMarkers(data = bayareaStations,
                   ~longitude,
                   ~latitude,
                   popup = bayareaStations$stationName,
                   radius = 1) %>%
  addCircleMarkers(data = minnStations,
                   ~lon,
                   ~lat,
                   popup = minnStations$name,
                   radius=1)
