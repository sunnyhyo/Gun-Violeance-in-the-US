#####################################
# Title : (2) Mapping
#####################################
# 패키지 설치
library(tidyverse)
select <- dplyr::select
count<-dplyr::count
rename<-dplyr::rename

# 경로지정
setwd("C:/Users/USER/Documents/Github/Gun-violence-in-the-US/JIIN")

#원자료
df1.data <- read_csv("data/DATA1.csv") 
df2.data <- read_csv("data/DATA2.csv")
df3.data <- read_csv("data/DATA3.csv")
df4.data <- read_csv("data/DATA4.csv")
code.data<-read_csv("data/UScode.csv") #51개주 위도 경도 코드 인구(2017)

#원자료 복사
df1 <- df1.data
df2 <- df2.data
df3 <- df3.data
df4 <- df4.data

#################################################33
#2. 위치별 사건 수, 희생자 수 비교
#1) 인구대비 주별 총기사건 수 비교
#1-a) 주별 차이 차트
#1-b) 주별 차이 지도 시각화
#2) 인구대비 주별 총기사건 희생자 수 비교
#2-a) 사건 수 대비 희생자 수(severity) 
#2-b) 인구 수 대비 희생자 수 
#3) 가설 - 총기 규제가 엄격한 주는 인구대비 총기사건 수 적을 것이다.
#1-b와 총기규제데이터 지도 비교
#외부데이터 활용
#총기규제데이터 
#https://www.washingtonpost.com/graphics/2017/national/assault-weapons-laws/?noredirect=on&utm_term=.ecdd99e8d448
#############################

#지도불러오기
#map_data
usmap<-map_data("state") 
head(usmap)
usmap %>% count(group)
#기본 지도 
ggplot(usmap, aes(long,lat)) +
  geom_polygon(aes(group=group), fill=NA, color="grey50")+
  coord_quickmap()+
  theme_void()

#long, lat 위도 경도 변수 통일
head(df1)
df1 <- df1 %>%
  rename(long=longitude, lat=latitude) 
usmap %>% count(region) 

#acc 주별(51개) 사건 수 변수 
a <- df1 %>% 
  count(state) %>% 
  rename(acc=n) 
head(a)

#state code lat long pop(인구)
code<- code.data[,c(1,2,4,5,6)]
head(code)

#state code lat long pop acc
b<-full_join(code,a)
head(b)

#############
#주별 geom_point()
ggplot(b, aes(long, lat))+ 
  geom_polygon( aes(group=group), data=usmap, fill=NA, color="grey50")+
  geom_point( aes(size=acc), colour="red", alpha=0.6)+
  scale_size_area(breaks=c(2500, 5000, 7500,10000), 
                  name="Number of \n accidents")+
  coord_quickmap()+
  theme_void()

#scale_size_area 조정- 분포확인
a %>%
  ggplot(aes(state, acc))+ geom_bar(stat="identity")

#####################
#인구대비 사고 수 많은 주 확대지도
#acc_pop 인구대비 사건 비율
b %>% mutate(acc_pop=acc/pop) %>%
  arrange(desc(acc_pop)) %>% 
  select(state, acc_pop) #district of columbia(DC)


b %>% mutate(acc_pop=acc/pop) %>%
  arrange(desc(acc_pop)) %>% 
  ggplot(aes(state, acc_pop))+
  geom_bar(stat="identity")

#DC 정보 code lat long pop acc
b %>% filter(state=="District of Columbia") 

#DC발생 총기사고 long, lat 뽑아냄
c<-df1 %>% 
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


