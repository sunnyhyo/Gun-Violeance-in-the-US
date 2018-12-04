#Geocomputation with R
#8. Making maps with R
#패키지 설치
install.packages("sf")
install.packages("raster")
install.packages("spData")
install.packages("spDataLarge")
install.packages("tmap")
install.packages("leaflet")
#패키지 불러오기
library(spData)
#library(spDataLarge)
library(dplyr)
library(sf)
library(raster)
library(tmap)
library(leaflet)

########
data("World")
names(World)
World$name
us<-World[World$name == "United States",]
us$geometry

tm_shape(us)

#지도불러오기 map_data
usmap<-map_data("state") 
head(usmap)

usmap %>%
  leaflet() %>%
  setView(zoom=12)

leaflet(usmap) %>%
  addTiles() %>%
  addPolygons()
  
