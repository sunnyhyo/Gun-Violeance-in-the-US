#################Setting code#####################
# 패키지 설치
library(tidyverse)
library(dplyr)
select <- dplyr::select
count<-dplyr::count
rename<-dplyr::rename
library(readr)
library(ggplot2)
library(viridis)
library(ggmap)

# 경로지정
setwd("C:/Users/USER/Documents/Github/Gun-violence-in-the-US/JIIN")

# 원자료
df1.data <- read_csv("data/DATA1.csv") 
df2.data <- read_csv("data/DATA2.csv")
df3.data <- read_csv("data/DATA3.csv")
df4.data <- read_csv("data/DATA4.csv")
code.data<-read_csv("data/UScode.csv") #51개주 위도 경도 코드 인구(2017)

# 원자료 복사 
df1 <- df1.data
df2 <- df2.data
df3 <- df3.data
df4 <- df4.data
code <- code.data

#지도불러오기 map_data
usmap<-map_data("state") 

# google map api key 코드 불러오기
if(!requireNamespace("devtools")) 
  install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
ggmap(get_googlemap())
register_google(key  ="AIzaSyBetNxF3HvdWiER0i_lIHm3tAu2rj4mWpE")

#############################################################
head(usmap)
usmap %>% count(group)
usmap %>% count(region) 

( df1 <- df1 %>%
    rename(long=longitude, lat=latitude) ) # long, lat 변수 통일

( a <- df1 %>% 
    count(state) %>% 
    rename(acc=n) )         # acc 주별(51개) 사건 수 변수 

code <- code[,c(1,2,4,5,6)]
head(code)                  # state code lat long pop(인구)

( b <- full_join(code,a) )  

( b <- b %>%                # b - state code lat long pop acc acc_pop
    filter(!is.na(acc), !is.na(pop)) %>%
    mutate( acc_pop = round( (acc/pop)*100000 ) )  )     # acc_pop accidents per 100000 

b %>% arrange(desc(acc_pop)) # district of columbia(DC)

b %>% mutate(acc_pop=acc/pop) %>%
  arrange(desc(acc_pop)) %>%
  filter()
  ggplot(aes(state, acc_pop))+
  geom_bar(stat="identity")

#DC 정보 code lat long pop acc
b %>% filter(state=="District of Columbia") 

#DC발생 총기사고 long, lat 뽑아냄
c <- df1 %>% 
  filter(state=="District of Columbia") %>%
  select(long, lat) 

#DC 지도
dc<-map_data('county', region='district of columbia')
head(dc)

#인구대비 사고 수 많은 주 확대지도
ggplot(dc, aes(long,lat))+
  geom_polygon(aes(group=group),fill="lightblue",colour="grey50")+
  geom_point(aes(long,lat), data=c, colour="red", alpha=0.2)+
  coord_quickmap()

library(tidyverse)
b %>% filter(state=="District of Columbia")
########################## Using google map ######
library("ggmap")
#install.packages("devtools")
library(devtools)
install_github("dkahle/ggmap")

if(!requireNamespace("devtools")) 
  install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
ggmap(get_googlemap())
register_google(key  ="AIzaSyBetNxF3HvdWiER0i_lIHm3tAu2rj4mWpE")

DC<-get_map("District of Columbia", zoom=12,
            maptype = "terrain")
ggmap(DC, extent="device") + 
  theme_void()+
  geom_point(aes(long,lat), data=c, colour="red", alpha=0.15)+
  ggtitle("District of Columbia") 
