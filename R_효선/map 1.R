library(tidyverse)
library(maps)
count<- dplyr::count


mi_countries <- map_data("county", "michigan") %>% 
  select(lon = long, lat, group, id = subregion)

head(mi_countries)

ggplot(mi_countries, aes(lon, lat)) + 
  geom_polygon(aes(group = group)) +
  coord_quickmap()

ggplot(mi_countries, aes(lon, lat)) + 
  geom_polygon(aes(group = group), fill = NA, color = "grey50") +
  coord_quickmap()

#####
# install.packages("USAboundaries")
# install.packages("sf")
# library(USAboundaries)
# library(sf)


##########
# point metadata
mi_cities <- maps::us.cities %>% 
  tbl_df() %>% 
  filter(country.etc == "MI") %>% 
  select(-country.etc, lon = long) %>% 
  arrange(desc(pop))
mi_cities

ggplot(mi_cities, aes(lon, lat)) +
  geom_point(aes(size = pop)) +
  scale_size_area() +
  coord_quickmap()

ggplot(mi_cities, aes(lon, lat)) + 
  geom_polygon(aes(group = group), mi_countries, fill = NA, color = "grey50") +
  geom_point(aes(size = pop), color = "red") + 
  scale_size_area() +
  coord_quickmap()

ggplot(mi_cities, aes(lon, lat)) + 
  geom_polygon(aes(group = group), mi_countries, color = "grey50") + 
  
  
ggplot(mi_countries, aes(lon, lat, group = group)) + 
  geom_polygon(aes(fill = pop), data = mi_cities)



###############

# 주별 사고 수
# 주별 인구 수

# 사건 - 위, 경도
# 

a <- df1 %>% count(state) %>% rename(      acc = n)  
a$region<- tolower(a$region)
usmap <- map_data("state")

table(a$region)
table(usmap$region)
names(df1)

df1<- df1 %>% rename(long = longitude, lat = latitude)

#####이거!!!!######
ggplot(usmap, aes(long, lat)) + 
  geom_polygon(aes(group = group), fill = NA, color = "grey50") +
  geom_point(data= df1, color = "red") + 
  coord_quickmap()


table(df1$state)
table(df1$congressional_district)
table(df1$state_house_district)
table(df1$state_senate_district)


ggplot(mi_cities, aes(lon, lat)) +  
  geom_polygon(aes(group = group), mi_countries, fill = NA, color = "grey50") +  # 미시간 주의 지도
  geom_point(aes(size = pop), color = "red") + 
  scale_size_area() +
  coord_quickmap()

code <- code[,c(1,2,4,5,6)]
oh <- left_join(code, a)



ggplot(oh, aes(long, lat)) +  
  geom_polygon(aes(group = group), usmap, fill = NA, color = "grey50") +  # 미시간 주의 지도
  geom_point(aes(size = acc), color = "red") + 
  scale_size_area() +
  coord_quickmap()

library(sf)
library(maps)
?st_read
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
states <- sf::st_as_sf(map("state", plot = FALSE, fill = TRUE))

?st_as_sf
ggplot() +
  geom_sf(data = nc)
ggplot() +
  geom_sf(aes(fill = AREA), data = nc, colour = "white")
ggplot() +
  geom_sf(data = states) + 
  geom_sf(data = nc)

library(sf)
library(dplyr)
library(ggplot2)
library(magrittr)
cd <- st_read('congressional_districts.shp', stringsAsFactors = FALSE)
head(cd)

