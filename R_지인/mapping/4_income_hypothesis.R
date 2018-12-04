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
code.data <- read_csv("data/UScode.csv") #51개주 위도 경도 코드 인구(2017)
income.data <- read_csv("data/USincome.csv")

# 원자료 복사 
df1 <- df1.data
df2 <- df2.data
df3 <- df3.data
df4 <- df4.data
code <- code.data
income<- income.data

####################### Income hypothesis #######################
# "소득이 낮은 지역일수록 총기사고가 더 많이 일어날 것이다."
# 2017년 미국 주별 연간소득 데이터 불러오기
names(income)
( income <- income %>% 
    filter( linecode == "3") %>%
    select(state, income) ) 

# b - state code lat long pop acc acc_pop
b
( acc_inc <- full_join(b, income, by="state") %>%
  select( state, acc_pop, income) )

# income 소득과 acc_pop 인구대비 사건수 상관관계를 확인해보기 위해 산점도를 그려보았다. 
summary(b$acc_pop)
acc_inc %>% filter(acc_pop<150) %>%
  ggplot(aes(acc_pop, income)) + 
  geom_point()
acc_inc %>% filter(acc_pop<150) %>%
  ggplot(aes(acc_pop, income)) + 
  geom_point() +geom_smooth(method = "lm")
# 산점도와 lm 곡선을 그려본 결과,
# 소득과 인구대비 총기사고 수간의 상관관계가 없어 보인다.
# 추가적으로 회귀분석 결과를 확인해보자.
cor(acc_inc$income, acc_inc$acc_pop, method = "pearson")
income.reg <- lm(acc_inc$acc_pop ~ acc_inc$income)
summary(income.reg)
# 회귀 분석 결과 결정계수가 0.097로 매우 작으므로
# 둘 간의 상관관계가 없다고 할 수 있다.
