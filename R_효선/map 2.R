##################
# Mapping

library(tidyverse)
library(ggplot2)
us_map <- map_data("state")
head(us_map, 3)


us_map %>% 
  filter(region %in% c("north carolina", "south carolina")) %>%
  ggplot(aes(x = long, y = lat)) +
  geom_point()

us_map %>% 
  filter(region %in% c("north carolina", "south carolina")) %>%
  ggplot(aes(x = long, y = lat)) +
  geom_path()

us_map %>% 
  filter(region %in% c("north carolina", "south carolina")) %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_path()

us_map %>% 
  filter(region %in% c("north carolina", "south carolina")) %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "black")

us_map %>% 
  filter(region %in% c("north carolina", "south carolina")) %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "black") + 
  theme_void()


us_map %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "black") + 
  theme_void()

data(votes.repub)
head(votes.repub)


library(dplyr)
library(viridis)

votes.repub %>%
  tbl_df() %>%
  mutate(state = rownames(votes.repub),
         state = tolower(state)) %>%
  right_join(us_map, by = c("state" = "region")) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = `1976`)) +
  geom_polygon(color = "black") + 
  theme_void() + 
  scale_fill_viridis(name = "Republican\nvotes (%)")

################
# 위치와 사고데이터 매핑
# names(us_map)
# right_join(code, us_map, by = c("state"= "region")) %>% 
#   ggplot(aes(long.x, lat.y, group = group, fill = pop2017))+
#   geom_polygon(color = "black")+
#   theme_void() + 
#   scale_fill_viridis(name = "Republican\nvotes (%)")



################
library(readr)
serial <- read_csv(paste0("https://raw.githubusercontent.com/",
                          "dgrtwo/serial-ggvis/master/input_data/",
                          "serial_podcast_data/serial_map_data.csv"))
head(serial, 3)


serial <- serial %>%
  mutate(long = -76.8854 + 0.00017022 * x,
         lat  = 39.23822 + 1.371014e-04 * y,
         tower = Type == "cell-site")
serial %>%
  slice(c(1:3, (n() - 3):(n())))


maryland <- map_data('county', region = 'maryland')
head(maryland)

baltimore <- maryland %>%
  filter(subregion %in% c("baltimore city", "baltimore"))
head(baltimore, 3)


ggplot(baltimore, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "lightblue", color = "black") + 
  theme_void()

# Washington
dc <- map_data("county", region = "district of columbia")
dc1 <- dc %>% 
  filter(subregion %in% c("washington"))
head(dc1)

ggplot(dc1, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "black")


ggplot(baltimore, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "lightblue", color = "black") + 
  geom_point(data = serial, aes(group = NULL, color = tower)) + 
  theme_void() + 
  scale_color_manual(name = "Cell tower", values = c("black", "red"))
  

########
## install.packages("ggmap")
library(ggmap)
beijing <- get_map("Beijing", zoom = 12)

ggmap(beijing)

ggmap(beijing) + 
  theme_void() + 
  ggtitle("Beijing, China")


if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)
register_google
register_google(key  ="AIzaSyB8Y12ytwQozu354z9-UHl4SadEttxngPE")



map_1 <- get_map("Estes Park", zoom = 12,
                 source = "google", maptype = "terrain") %>%
  ggmap(extent = "device")

map_2 <- get_map("Estes Park", zoom = 12,
                 source = "stamen", maptype = "watercolor") %>%
  ggmap(extent = "device")

map_3 <- get_map("Estes Park", zoom = 12,
                 source = "google", maptype = "hybrid") %>%
  ggmap(extent = "device")

library(gridExtra)
grid.arrange(map_1, map_2, map_3, nrow = 1) 


get_map(c(2.35, 48.86), zoom = 10) %>%
  ggmap(extent = "device")

get_map("Baltimore County", zoom = 10, 
        source = "stamen", maptype = "toner") %>%
  ggmap() + 
  geom_polygon(data = baltimore, aes(x = long, y = lat, group = group),
               color = "navy", fill = "lightblue", alpha = 0.2) + 
  geom_point(data = serial, aes(x = long, y = lat, color = tower)) + 
  theme_void() + 
  scale_color_manual(name = "Cell tower", values = c("black", "red"))


geocode("Supreme Court of the United States")


install.packages("choroplethr")
install.packages("choroplethrMaps")
library(choroplethr)
library(choroplethrMaps)
data(df_pop_county)
df_pop_county %>% slice(1:3)
