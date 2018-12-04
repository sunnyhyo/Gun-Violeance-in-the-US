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

# google map api key 코드 불러오기
if(!requireNamespace("devtools")) 
  install.packages("devtools") 
devtools::install_github("dkahle/ggmap", ref = "tidyup")
ggmap(get_googlemap())
register_google(key  ="AIzaSyBetNxF3HvdWiER0i_lIHm3tAu2rj4mWpE")

# 경로지정
setwd("C:/Users/USER/Documents/Github/Gun-violence-in-the-US/JIIN")

# 원자료
df1.data <- read_csv("data/DATA1.csv") 
df2.data <- read_csv("data/DATA2.csv")
df3.data <- read_csv("data/DATA3.csv")
df4.data <- read_csv("data/DATA4.csv")
code.data <- read_csv("data/UScode.csv") #51개주 위도 경도 코드 인구(2017) 규제

# 원자료 복사 
df1 <- df1.data
df2 <- df2.data
df3 <- df3.data
df4 <- df4.data
code <- code.data

#지도불러오기 map_data
usmap <-map_data("state") 

################# Mapping code #####################
# 어느 주에서 (인구대비) 총기사고가 많이 발생했는가
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

# 인구 수를 고려했을 때 사건 수 - 인구 100000명 당 사고 발생 횟수
# accidents per 100000:  acc_pop  
( b <- b %>%                # b - state code lat long pop acc acc_pop
    filter(!is.na(acc), !is.na(pop)) %>%
    mutate( acc_pop = round( (acc/pop)*100000 ) )  )    

( usmap1 <- usmap %>%
  tbl_df %>%
  rename(state = region) %>%    # state 변수 통일 
  select(long:state) )

( b1 <- b %>%
  mutate( state = tolower(state) ) %>%
  select( state, acc, acc_pop ) )             # b1 - state acc acc_pop

( b2 <- right_join(usmap1, b1, by="state") )  # b2 - usmap + acc acc_pop 

################## Mapping code ###################
# figure 1.1) Incidents by states 주별 사고 수 지도 - size옵션
ggplot(b2, aes(long, lat))+ 
  geom_polygon( aes(group=group), data=usmap, 
                fill=NA, color="grey50")+
  geom_point( aes(long,lat, size=acc), 
              data=b, colour="red", alpha=0.6)+
  scale_size_area(breaks=c(2000,4000,6000,10000),
                  name="Number of \n incidents")+
  labs(title = "The number of incidents by states")+
  coord_quickmap()+
  theme_void()+
  theme( plot.title = element_text(hjust=0.5, vjust=-1,
                                   colour="#4e4d47") )

# 주별 사고수 편차가 크기 때문에 scale조정이 필요.
summary(b2$acc) 

# scale 조정
quantiles <- quantile(b2$acc,
         probs=seq(0, 1,length.out = 6+1))
labels<- c() 
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2),
                             "-",
                             round(quantiles[idx+1], 2)))
}
labels <- labels[1:length(labels)-1] # 사고수 6개 범주로 나눔
b2$acc_quantiles <- cut(b2$acc,
                        breaks = quantiles,
                        labels = labels,
                        include.lowest = T)
# 총기사고 수 많은 상위 3개 주
b %>% arrange(desc(acc)) %>%  select(state, pop, acc, acc_pop)
( text<- b %>% 
    filter(state %in% c("Illinois","California","Florida")) %>% 
    select(state, lat, long) )


# figure 1.2) Incidents by states 주별 사고 수 지도 - fill옵션 
b2 %>%
  ggplot(aes(long, lat))+
  geom_polygon(aes(fill=acc_quantiles, group=group),
               colour="grey50", size=0.1)+
  geom_label(aes(long, lat, label=state), data=text,
             fontface = "bold")+
  coord_equal()+
  theme_void()+
  labs(title = "The number of incidents by states")+
  scale_fill_viridis( option = "magma",
                      name = "The number\nof incidents ",
                      discrete = TRUE,
                      direction = -1)+
  theme( plot.title = element_text(hjust=0.5, colour="#4e4d47" )) 

# 인구 수가 많을수록 총기 사고가 많을 수 있다.
# 주별 인구대비 사고 수 데이터 활용해보자.
head(b2)
summary(b2$acc_pop) # 주별 인구대비 사고 수 편차가 큼
# 인구 고려시, 사고 수가 많은 주가 달라진다. 
b %>% arrange(desc(acc)) %>%  select(state, pop, acc, acc_pop)
  
# 주별 편차 고려하여 scale 조정
quantiles1 <- quantile(b2$acc_pop,
                      probs=seq(0, 1,length.out = 6+1))
labels<- c() 
for(idx in 1:length(quantiles1)){
  labels <- c(labels, paste0(round(quantiles1[idx], 2),
                             "-",
                             round(quantiles1[idx+1], 2)))
}
labels <- labels[1:length(labels)-1]  
# 사고 수 6개 범주로 나눔: acc_pop_quantiles
b2$acc_pop_quantiles <- cut(b2$acc_pop,
                        breaks = quantiles1,
                        labels = labels,
                        include.lowest = T)

# Alaska 제외하고 시각화하였다.
b %>% arrange(desc(acc_pop)) %>%  select(state, pop, acc, acc_pop)
( text2<- b %>% 
    filter(acc_pop >72, state != "Alaska") %>% 
    select(state, lat, long) )
b2 %>%
  ggplot(aes(long, lat)) +
  geom_polygon(aes(fill=acc_pop_quantiles, group=group),
               colour="grey50", size=0.1) +
  geom_text(aes(long, lat, label=state), data=text2,
             colour="grey70", fontface = "bold") +
  coord_equal() +
  theme_void() +
  labs( title = "The number of accidents by states",
        subtitle = "considering population size" ) +
  scale_fill_viridis( option = "magma",
                      name = "The number\nof accident",
                      discrete = TRUE,
                      direction = -1 ) +
  theme( plot.title = element_text(hjust=0.5, colour="#4e4d47") ,
         plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47") ) 


