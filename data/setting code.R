######################################
# Title : setting code
# 이 문서는 앞으로 분석 시 패키지를 설치하고, 자료를 불러오는데 사용하는 문서입니다. 
# 새로운 세션에서 이 문서를 실행하고 분석을 시작하시면 됩니다. 

#####################################
# 패키지 설치
library(tidyverse)
select <- dplyr::select

# 경로지정
setwd("C:/Users/HS/Documents/GitHub/Gun-Violeance-in-the-US")

# 원자료
df1.data <- read_csv("data/DATA1.csv") 
df2.data <- read_csv("data/DATA2.csv")
df3.data <- read_csv("data/DATA3.csv")
df4.data <- read_csv("data/DATA4.csv")

# 원자료 복사 
# 복사된 데이터셋만 수정해서 분석해부세요
df1 <- df1.data
df2 <- df2.data
df3 <- df3.data
df4 <- df4.data


## 참고 ##
# key 확인
# df1 %>% count(incident_id) %>% filter(n>1)  # primary key
# df2 %>% group_by(incident_id) %>% count(pct_id) %>% filter(n>1) # 각 incident 별로 pct_id는 유일
# df3 %>% count(incident_id) %>% filter(n>1)  # primary key
# df4 %>% count(incident_id) %>% filter(n>1)  # incident_id 로 data1, data2와 연결

# join 
# 
# df1 %>% left_join(df3)
# df1 %>% full_join(df3)
# 
# df1 %>% left_join(df2)
# df1 %>% full_join(df2)
# 
# df1 %>% left_join(df4)
# df1 %>% full_join(df4)
# 
# df1 %>% left_join(df3) %>% left_join(df2)
