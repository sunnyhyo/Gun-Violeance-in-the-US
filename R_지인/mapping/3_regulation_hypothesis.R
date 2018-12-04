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

################## gun regulation hypothesis ###########################
# 주별로 시행하고 있는 총기 규제가 다르다. 
# UScode data의 reg 변수는 US gun regulation data (2018.02)
# '7가지 총기규제 법안 중 몇개를 시행하고 있는가'의 정보를 담고 있다.
# 시행하고 있는 법안의 갯수가 많을수록 총기규제가 엄격한 편이라 가정했을 때,
# Hypothesis 1) "총기 규제가 엄격할 수록 (더 많은 법안을 시행하고 있을수록) 총기사고가 적게 일어날 것이다" 
# 위의 가설을 확인해보고자 한다.
reg <- code.data %>% select(state, reg) %>% mutate(state=tolower(state))
( b2 <- full_join(b2, reg, by="state") )
reg %>% arrange(desc(reg)) 
( text3<- b %>% 
    filter(state %in% c("California", "Connecticut") ) %>% 
    select(state, lat, long) )

# California와 Connecticut 주가 7개의 법률 중 7개 모두 시행하고 있다.
b2 %>%
  ggplot(aes(long, lat)) +
  geom_polygon(aes(fill=reg, group=group),
               colour="grey50", size=0.1) +
  geom_label(aes(long, lat, label=state), data=text3, fontface = "bold") +
  coord_equal() +
  theme_void() +
  labs( title = "The number of regulations by states",
        subtitle =  "seven types of gun control enacted at the state level ")+
  scale_fill_viridis( option = "magma",
                      name = "The number\nof regulations",
                      direction = -1 ) +
  theme( plot.title = element_text(hjust=0.5, colour="#4e4d47") ,
         plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47") ) 


# 총기 규제와 인구대비 사고 수 간의 관련성을 알아보기 위해 geom_smooth를 그려보았다.
reg2 <- code.data %>% select(state, reg)
( b <- full_join(b, reg2, by="state") )
b2 %>% filter(acc_pop<300) %>%
  ggplot(aes(reg, acc_pop)) + geom_smooth(method = "lm") +
  geom_point()
# geom_smooth 그래프에서 둘 간의 아주 약한 음의 상관관계가 있어 보여 
# 회귀분석 결과와 상관계수를 확인해 보았다.
cor(b2$acc_pop, b2$reg, method = 'pearson')
reg.reg <- lm( b2$acc_pop ~ b2$reg)
summary(reg.reg) 
# 회귀분석 결과, 결정계수가 0.058로 0에 가까우므로 
# 총기 규제와 인구대비 총기사고 수 간의 관계가 없다고 할 수 있다.
