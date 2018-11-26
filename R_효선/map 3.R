library(maps)  # us cities
library(tidyverse)
library(ggmap)
# install.packages("tidyverse")
# install.packages("glue")


# 그림을 그리기 위해서 위치 정보가 필요함
# boundary 자료도 들고가기

mi_counties <- map_data("county", "michigan") %>% 
  select(lon = long, lat, group, id = subregion)
head(mi_counties)


ggplot(mi_counties, aes(lon, lat)) +
  geom_polygon(aes(group = group)) + 
  coord_quickmap()

ggplot(mi_counties, aes(lon, lat)) +
  geom_polygon(aes(group = group), fill = NA, colour = "grey50") + 
  coord_quickmap()


library(USAboundaries) # 미국 모든 주의 경계정보
library(sf)
#> Warning: package 'sf' was built under R version 3.4.4
#> Linking to GEOS 3.6.1, GDAL 2.2.0, proj.4 4.9.3
# c18 <- us_boundaries(as.Date("1820-01-01"))
# c18df <- fortify(c18)
# head(c18df)
c18df<-NULL
for(i in 1:nrow(c18)){
  sel<-length(c18$geometry[[i]])
  for(j in 1:sel){
    longlat<-c18$geometry[[i]][[j]][[1]]
    n<-nrow(longlat)
    temp<-data.frame(long=longlat[,1],lat=longlat[,2],order=1:n,
                     id=c18$abbr_name[i],group=paste(c18$abbr_name[i],".",j,sep=""))
    c18df<-rbind(c18df,temp)
  }   
}


ggplot(c18df, aes(long, lat)) + 
  geom_polygon(aes(group = group), colour = "grey50", fill = NA) +
  coord_quickmap()

plot(st_geometry(us_boundaries("1820-01-01")))


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
  geom_polygon(aes(group = group), mi_counties, fill = NA, colour = "grey50") +
  geom_point(aes(size = pop), colour = "red") + 
  scale_size_area() + 
  coord_quickmap()


# Raster images
if (file.exists("mi_raster.rds")) {
  mi_raster <- readRDS("mi_raster.rds")
} else {
  bbox <- c(
    min(mi_counties$lon), min(mi_counties$lat), 
    max(mi_counties$lon), max(mi_counties$lat)
  )
  location<-c(median(mi_cities$lon),median(mi_cities$lat))
  mi_raster <- get_map(location=location,zoom=7)
  saveRDS(mi_raster, "mi_raster.rds")
}
ggmap::ggmap(mi_raster)
ggmap::ggmap(mi_raster) + 
  geom_point(aes(size = pop), mi_cities, colour = "red") + 
  scale_size_area()


mi_census <- midwest %>%
  tbl_df() %>%
  filter(state == "MI") %>% 
  mutate(county = tolower(county)) %>%
  select(county, area, poptotal, percwhite, percblack)
mi_census

census_counties <- left_join(mi_census, mi_counties, by = c("county" = "id"))
census_counties


ggplot(census_counties, aes(lon, lat, group = county)) + 
  geom_polygon(aes(fill = poptotal)) + 
  coord_quickmap()

ggplot(census_counties, aes(lon, lat, group = county)) + 
  geom_polygon(aes(fill = percwhite)) + 
  coord_quickmap()



