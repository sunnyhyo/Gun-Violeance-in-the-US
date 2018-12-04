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
code.data <- read_csv("data/UScode.csv") #51개주 위도 경도 코드 인구(2017)

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
# 사건별 희생자 수 상위 100

# 사건별 심각도 점수화 
#(전체희생자대비 n_killed/ raster)
names(df1)
names(df3)

( a <- df1 %>% 
  select("incident_id", "state", "longitude", "latitude") %>%
  rename(long="longitude", lat="latitude") %>%
  mutate(state = tolower(state)) )
( b <- df3 %>%
    select("incident_id", "n_Victim","n_Victim_Injured", "n_Victim_Killed") )

( c <- full_join(a,b, by="incident_id") ) 

library(dplyr)
(c <- c %>% 
  filter(!is.na(n_Victim),
         !is.na(n_Victim_Injured), 
         !is.na(n_Victim_Killed) ) %>%
  group_by(state) %>%
  mutate(severity = n_Victim_Killed / n_Victim) )
summary(c$severity)
summarize( sumVic = sum(n_Victim),
             sum)
  




  group_by(state)
c <- c[,c(2:6)]

c <- c %>% 
  filter(!is.na(severity)) 
mutate(severity= n_Victim_Killed/n_Victim)

install.packages("nycflights13")
head(c) #long lat severity
summary(c$severity)
c %>%
  arrange(severity) %>%
  
  ggplot(aes(incident_id, severity)) +
  geom_boxplot(aes(group=state))
?cut_width
?quantile
quantile(c$severity, probs=seq(0, 1, length.out = 7))

usmap1 <- usmap %>%
  tbl_df %>%
  rename(state = region) %>%
  select(long:state)
head(usmap1)


