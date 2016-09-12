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


cook_tracts <- tracts(state = 'IL', county = "Cook", cb=TRUE)

nyc_tracts <- tracts(state="NY", county = c("New York", "Kings", "Bronx", "Richmond", "Queens"))

bayarea_tracts <- tracts(state="CA", county = c("Santa Clara", "San Francisco"))

minn_tracts <- tracts(state="MN", county = "Hennepin")



#####################COOK###############################
#read and parse api
api <- paste0("http://db.datausa.io/api/csv/?show=geo&sumlevel=tract&required=income&geo=16000US1714000&year=latest")
cook <- api %>% read.csv() 



#change data type, spilt census tract into mergeable GEOID
cook$geo <- as.character(cook$geo)
split <- data.frame(str_split_fixed(cook$geo, "S",2))
cook <- cbind(cook, split$X2)

#Change column names for consistency and reproducibility
names(cook)[5] <- "GEOID"
names(cook)[4] <- "metric"

cook$GEOID <- as.numeric(as.character(cook$GEOID))

#read in shapefile
cook_tracts <- tracts(state="IL", county = "Cook")
api.key.install(key="a2ab1e3ad2954d0ac5e340c3491a3220f10d732f")
cook_geo<-geo.make(state="IL", county = c("Cook"), tract=cook$GEOID)

#merge with dataframe
cook_merged<- geo_join(cook_tracts, cook, "GEOID", "GEOID")
cook_filtered <- cook_merged[cook_merged$GEOID %in% cook$GEOID,]
cook_filtered$metric <- as.numeric(as.character(cook_filtered$metric))
#create information for map - CHANGE STRINGS BELOW
cook_popup <- paste0("GEOID: ", cook_filtered$GEOID, "<br>", clean_name, ": ", cook_filtered$metric)
cook_pal <- colorNumeric(
  palette = "Reds",
  domain = cook_filtered$metric)

map<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cook_filtered, 
              fillColor = ~pal(cook_filtered$metric), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup)

#########################################################


#####################NYC###############################
#read and parse api
api <- paste0("http://db.datausa.io/api/csv/?show=geo&sumlevel=tract&required=income&geo=16000US3651000&year=latest")
nyc <- api %>% read.csv() 



#change data type, spilt census tract into mergeable GEOID
nyc$geo <- as.character(nyc$geo)
split <- data.frame(str_split_fixed(nyc$geo, "S",2))
nyc <- cbind(nyc, split$X2)

#Change column names for consistency and reproducibility
names(nyc)[5] <- "GEOID"
names(nyc)[4] <- "metric"

nyc$GEOID <- as.numeric(as.character(nyc$GEOID))

#read in shapefile
nyc_tracts <- tracts(state="NY", county = c("New York", "Kings", "Bronx", "Richmond", "Queens"))
api.key.install(key="a2ab1e3ad2954d0ac5e340c3491a3220f10d732f")
nyc_geo<-geo.make(state="NY", county = c("New York", "Kings", "Bronx", "Richmond", "Queens"), tract=nyc$GEOID)

#merge with dataframe
nyc_merged<- geo_join(nyc_tracts, nyc, "GEOID", "GEOID")

nyc_filtered <- nyc_merged[nyc_merged$GEOID %in% nyc$GEOID,]

nyc_filtered$metric <- as.numeric(as.character(nyc_filtered$metric))
#create information for map - CHANGE STRINGS BELOW
nyc_popup <- paste0("GEOID: ", nyc_filtered$GEOID, "<br>", clean_name, ": ", nyc_filtered$metric)
nyc_pal <- colorNumeric(
  palette = "Reds",
  domain = nyc_filtered$metric)

map<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = nyc_filtered, 
              fillColor = ~nyc_pal(nyc_filtered$metric), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = nyc_popup)

#########################################################

###################BAY AREA#########################
#read and parse api
api1 <- paste0("http://db.datausa.io/api/csv/?show=geo&sumlevel=tract&required=income&geo=16000US0655282&year=latest")
bayarea1 <- api1 %>% read.csv() 
api2 <- paste0("http://db.datausa.io/api/csv/?show=geo&sumlevel=tract&required=income&geo=16000US0667000&year=latest")
bayarea2 <- api2 %>% read.csv() 
api3 <- paste0("http://db.datausa.io/api/csv/?show=geo&sumlevel=tract&required=income&geo=16000US0668000&year=latest")
bayarea3 <- api2 %>% read.csv() 

bayarea <- rbind(bayarea1, bayarea2, bayarea3)
str(bayarea)
#change data type, spilt census tract into mergeable GEOID
bayarea$geo <- as.character(bayarea$geo)
split <- data.frame(str_split_fixed(bayarea$geo, "S",2))
bayarea <- cbind(bayarea, split$X2)

#Change column names for consistency and reproducibility
names(bayarea)[5] <- "GEOID"
names(bayarea)[4] <- "metric"

bayarea$GEOID <- as.numeric(as.character(bayarea$GEOID))

#read in shapefile
bayarea_tracts <- tracts(state="CA", county = c("Santa Clara", "San Francisco"))
api.key.install(key="a2ab1e3ad2954d0ac5e340c3491a3220f10d732f")
bayarea_geo<-geo.make(state="CA", county = c("Santa Clara", "San Francisco"), tract=bayarea$GEOID)
bayarea$GEOID <- paste0(0, bayarea$GEOID)

#merge with dataframe
bayarea_merged<- geo_join(bayarea_tracts, bayarea, "GEOID", "GEOID")
bayarea_filtered <- bayarea_merged[bayarea_merged$GEOID %in% bayarea$GEOID,]
bayarea_filtered$metric <- as.numeric(bayarea_filtered$metric)
#create information for map - CHANGE STRINGS BELOW
bayarea_popup <- paste0("GEOID: ", bayarea_filtered$GEOID, "<br>", clean_name, ": ", bayarea_filtered$metric)
bayarea_pal <- colorNumeric(
  palette = "Reds",
  domain = bayarea_filtered$metric)

map<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = bayarea_filtered, 
              fillColor = ~bayarea_pal(bayarea_filtered$metric), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = bayarea_popup)
#################################################

###################MINNEAPOLIS#########################
#read and parse api
api1 <- paste0("http://db.datausa.io/api/csv/?show=geo&sumlevel=tract&required=income&geo=16000US2743000&year=latest")
minn1 <- api1 %>% read.csv() 
api2 <- paste0("http://db.datausa.io/api/csv/?show=geo&sumlevel=tract&required=income&geo=16000US2758000&year=latest")
minn2 <- api2 %>% read.csv() 

minn <- rbind(minn1, minn2)

#change data type, spilt census tract into mergeable GEOID
minn$geo <- as.character(minn$geo)
split <- data.frame(str_split_fixed(minn$geo, "S",2))
minn <- cbind(minn, split$X2)

#Change column names for consistency and reproducibility
names(minn)[5] <- "GEOID"
names(minn)[4] <- "metric"

minn$GEOID <- as.numeric(as.character(minn$GEOID))

#read in shapefile
minn_tracts <- tracts(state="MN", county = c("Hennepin", "Ramsey"))
api.key.install(key="a2ab1e3ad2954d0ac5e340c3491a3220f10d732f")
minn_geo<-geo.make(state="MN", county = c("Hennepin", "Ramsey"), tract=minn$GEOID)

#merge with dataframe
minn_merged<- geo_join(minn_tracts, minn, "GEOID", "GEOID")
minn_filtered <- minn_merged[minn_merged$GEOID %in% minn$GEOID,]
minn_filtered$metric <- as.numeric(minn_filtered$metric)
#create information for map - CHANGE STRINGS BELOW
minn_popup <- paste0("GEOID: ", minn_filtered$GEOID, "<br>", clean_name, ": ", minn_filtered$metric)
minn_pal <- colorNumeric(
  palette = "Reds",
  domain = minn_filtered$metric)

map<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = minn_filtered, 
              fillColor = ~minn_pal(minn_filtered$metric), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = minn_popup)
#################################################



#build map

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cook_filtered, 
              fillColor = ~cook_pal(cook_filtered$metric), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = cook_popup) %>%
  addPolygons(data = nyc_filtered, 
              fillColor = ~nyc_pal(nyc_filtered$metric), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = nyc_popup) %>%
  addPolygons(data = bayarea_filtered, 
              fillColor = ~bayarea_pal(bayarea_filtered$metric), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = bayarea_popup) %>%
  addPolygons(data = minn_filtered, 
              fillColor = ~minn_pal(minn_filtered$metric), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = minn_popup) %>%
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
