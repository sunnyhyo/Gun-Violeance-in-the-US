######################################
# Title : setting code
#####################################
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
# 복사된 데이터셋만 수정해서 분석해부세요
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
