library(tidyverse)
setwd("C:/Users/HS/Documents/GitHub/Gun-violence-in-the-US")
select <- dplyr::select

# DATA
# DATA1 : main data
# DATA2 : participant data
# DATA3 : df2 로 df3 만들기!!

# DATA1 
# incident_id key 확인
df1.data <- read_csv("data/DATA1_main.csv")
df1 <- df1.data
df1 %>% count(incident_id) %>% filter(n>1)  #DATA1 에서 incident_id는 primary index


# DATA2
# loading
df2.data <- read_csv("data/practice1.csv")
df2 <- df2.data
df2
# incident_id key 확인
df2 %>% count(incident_id) %>% filter(n>1)  # DATA2 에서 incident_id는 primary는 아니고 무슨키지? 무튼 1:다
# pct_id key 확인
df2 %>% group_by(incident_id) %>% count(pct_id) %>% filter(n>1) %>% print(n = 24)  #NA 가 중복되는 값이 있어 뭐지?
df2 %>% filter(incident_id == 313794)
df2 %>% filter(incident_id == 366264)

df2 %>% group_by(incident_id) %>% count(pct_id) 
df2 %>% group_by(incident_id)%>% filter(is.na(pct_id))
df2 %>% filter(is.na(pct_id))

df2 %>% count(incident_id)
df2 %>% group_by(incident_id) %>% count(incident_id) %>% filter(n>1)
# df2 %>% filter(incident_id == 176097)
df2 %>% group_by(incident_id) %>% count(type)
df2 %>% group_by(incident_id) %>% count(status)


df2 %>% group_by(incident_id) %>% count(pct_id)