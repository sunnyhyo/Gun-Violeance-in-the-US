#############
# title : gun violence 전처리
# author : 효선

# 패키지 설치
#install.packages("splitstackshape")
library(splitstackshape)
library(tidyverse)
library(plyr)

# 경로 설정
setwd("C:/Users/HS/Documents/GitHub/Gun-violence-in-the-US")


# 원본 데이터 불러오기
# gun.data 는 원본이므로 건드리지 말기
gun.data <- read_csv("data/gun-violence-data.csv")
gun.data <- gun.data[,c(1:29)]
names(gun.data)


####################
# PREPARE DATASETS
#main
del_variance <- c("address", "location_description", "notes", "sources" )
main.data <- gun.data %>% 
  select( -contains("url"), -starts_with("gun_"), -one_of(del_variance)) %>% 
  separate(date, c("year", "month", "day"), sep = "-") %>% 
  mutate(weekdays = weekdays(as.Date(gun.data$date), abbreviate = TRUE)) %>%   #우선 16개 완료
  filter(year == 2015 | year == 2016 | year == 2017 )
names(main.data)

#dim(gun.data)
#dim(main.data)
#names(main.data)
#head(main.data)

# DATA1 : main dataset
df1_mainset <- main.data %>% select(-starts_with("participant"), -incident_characteristics)

table(df1_mainset$year)
dim(df1_mainset)
names(df1_mainset)
write.csv(df1_mainset, "data/DATA1_main.csv", row.names = FALSE) #변수 15개



###############
# 전처리 필요한 변수 , || 분할
#onlygun.data <- gun.data %>% select(incident_id, starts_with("gun_"))
participant.data <- main.data %>% select(incident_id, starts_with("participant"), -participant_name, -participant_relationship)
#dim(participant.data)
character.data <- main.data %>% 
  select(incident_id, state, city_or_county, incident_characteristics)
#dim(character.data)

###########
# DATA4 : character
# replacing "||" with "|" as both separators are used
character.data$incident_characteristics <- gsub("\\|\\|", "|", main.data$incident_characteristics)
df4_charter <- as.tibble(cSplit(character.data, 'incident_characteristics', sep =  '|', direction="long"))
head(df4_charter)
numCat <- round(nrow(df4_charter)/nrow(gun.data),1)
cat('On average, there are', numCat, 'incident categories specified per incident')

#write.csv(df4_charter, "data/DATA4_character.csv", row.names = FALSE)
#head(character.data$incident_characteristics)
#head(gun.data$incident_characteristics)


############
#onlygun.data$gun_stolen <- gsub("\\|\\|", "|", main.data$gun_stolen)
#onlygun.data$gun_type <- gsub("\\|\\|", "|", main.data$gun_type)
#a <- head(cSplit(onlygun.data, c("gun_stolen"), sep = c("|"), direction = "long"))
#b <- cSplit(a, "gun_stolen", sep = "::")
#c <- cSplit(b, "gun_type", sep = "|", direction = "long") 
#d <- cSplit(c, "gun_type", sep = "::")

###########
#DATA2 : participant
names(participant.data)
participant.data %>% print(n=100)
participant.data %>% group_by(participant_age) %>% summarise(n=n())
participant.data %>% group_by(participant_age_group) %>% summarise(n=n())
participant.data %>% group_by(participant_gender) %>% summarise(n=n())
participant.data %>% group_by(participant_status) %>% summarise(n=n())
participant.data %>% group_by(participant_type) %>% summarise(n=n())



participant.data$participant_age <- gsub("\\|\\|", "|", main.data$participant_age)
participant.data$participant_age_group <- gsub("\\|\\|", "|", main.data$participant_age_group)
participant.data$participant_gender <- gsub("\\|\\|", "|", main.data$participant_gender)
participant.data$participant_status <- gsub("\\|\\|", "|", main.data$participant_status)
participant.data$participant_type <- gsub("\\|\\|", "|", main.data$participant_type)

a <- participant.data %>% select(1:2)
a1 <- as.tibble(cSplit(a, "participant_age", sep = "|", direction = "long"))
a2 <- as.tibble(cSplit(a1, "participant_age", sep = "::"))

b <- participant.data %>% select(1,3)
b1 <- as.tibble(cSplit(b, "participant_age_group", sep = "|", direction = "long"))
b2 <- as.tibble(cSplit(b1, "participant_age_group", sep = "::"))


c <- participant.data %>% select(1,4)
c1 <- as.tibble(cSplit(c, "participant_gender", sep = "|", direction = "long"))
c2 <- as.tibble(cSplit(c1, "participant_gender", sep = "::"))

d <- participant.data %>% select(1,5)
d1 <- as.tibble(cSplit(d, "participant_status", sep = "|", direction = "long"))
d2 <- as.tibble(cSplit(d1, "participant_status", sep = "::"))

e <- participant.data %>% select(1,6)
e1 <- as.tibble(cSplit(e, "participant_type", sep = "|", direction = "long"))
e2 <- as.tibble(cSplit(e1, "participant_type", sep = "::"))

a2 <- a2 %>% rename(pct_id = participant_age_1,
                    age = participant_age_2)
b2 <- b2 %>% rename(pct_id = participant_age_group_1,
                    age_group = participant_age_group_2)
c2 <- c2 %>% rename(pct_id = participant_gender_1,
                    gender = participant_gender_2)
d2 <- d2 %>% rename(pct_id = participant_status_1,
                    status = participant_status_2)
e2 <- e2 %>% rename(pct_id = participant_type_1,
                    type = participant_type_2)


df2_participant <- a2 %>% full_join(b2, key= c("incident_id","pct_id")) %>% 
  full_join(c2, key= c("incident_id","pct_id")) %>% 
  full_join(d2, key= c("incident_id","pct_id")) %>% 
  full_join(e2, key= c("incident_id","pct_id")) %>% arrange(incident_id)

df2_participant$pct_id <- as.integer(df2_participant$pct_id)
df2_participant$age <- as.numeric(df2_participant$age)

write.csv(df2_participant, "data/DATA2_participant.csv", row.names = FALSE)

###############################
df2_participant %>% count(pct_id) %>% print(n= 100)
df2_participant %>% count(age) %>% print(n= 100)
df2_participant %>% count(age_group) %>% print(n= 27)
df2_participant %>% count(gender) %>% print(n= 27)
df2_participant %>% count(status) %>% print(n= 100)
df2_participant %>% count(type) %>% print(n= 100)

#pct_id 
filter(df2_participant, pct_id == 99)
filter(main.data, incident_id == 577157) %>% select(participant_age)
filter(df2_participant, pct_id == 62)
filter(main.data, incident_id == 575663) %>% select(participant_age)
# 엑셀로 원자료 확인해보니 실제로 사상자가 많은 큰 규모의 사건이 있었음
# pct_id 가 큰 이유는 사상자 수가 많은 사건이었기 때문이다
# 대규모 사건인 경우 따로 보기 해야할듯

#age
#별다른 특이점 없다

#age_group
#adult 18+, child 0-11, teen 12-17 외에 
#male, female, subject - suspect 자료가 있음
filter(df2_participant, age_group == "Female")
filter(main.data, incident_id == 793800) %>% select(participant_age_group)
filter(df2_participant, age_group == "Male")
filter(main.data, incident_id == 394642) %>% select(participant_age_group)
filter(df2_participant, age_group == "Subject-Suspect")
filter(main.data, incident_id == 806071) %>% select(participant_age_group)
#확인결과 데이터 입력시에 오류로 확인되었다. 
table(is.na(df2_participant$age))
table(is.na(df2_participant$age_group))
#age와 age_group 결측치 차이가 약 7만개 정도라서 age_group 살려두었다. 

#gender 
#확인결과 데이터 입력시에 오류로 확인되었다. 
#famale, male, na 외의 값은 데이터 입력시 오류  -> na 처리

#status
#subject-suspect, victim 은 입력오류  -> na 처리

#type
#unharmed 입력오류  -> na 처리

df2_participant2 <- df2_participant

df2_participant2$age_group <- ifelse(df2_participant$age_group == "Male" |
         df2_participant$age_group == "Female"|
         df2_participant$age_group == "Subject-Suspect", NA, df2_participant$age_group)
df2_participant2 %>% count(age_group) #라벨몇 바꿔주기 

df2_participant2$gender <- ifelse(df2_participant$gender == "Male" |
                                    df2_participant$gender == "Female", df2_participant$gender, NA)
df2_participant2 %>% count(gender)

df2_participant2$status <- ifelse(df2_participant$status == "Subject-Suspect" |
                                    df2_participant$status == "Victim", NA, df2_participant$status)
df2_participant2 %>% count(status)

df2_participant2$type <- ifelse(df2_participant2$type == "Unharmed", NA, df2_participant2$type)
df2_participant2 %>% count(type)

dim(df2_participant)
dim(df2_participant2)
write.csv(df2_participant2, "data/DATA2_participant2.csv", row.names = FALSE)


#########################################################
View(a2)
View(b2)
View(c2)
View(d2)
View(e2)

dim(a2) 
dim(b2)
dim(c2)
dim(d2)
dim(e2)

dim(gun.data)


#write.csv(aa, "data1/aa.csv", row.names = FALSE)
#write.csv(bb, "data1/bb.csv", row.names = FALSE)
#write.csv(cc, "data1/cc.csv", row.names = FALSE)
#write.csv(dd, "data1/dd.csv", row.names = FALSE)
#write.csv(ee, "data1/ee.csv", row.names = FALSE)

#1,5,4,5,5
#2,9,5,9,10
aa <- head(a2, 2); aa
bb <- head(b2, 9); bb
cc <- head(c2, 5); cc
dd <- head(d2, 9); dd
ee <- head(e2, 10); ee


aa %>% full_join(bb, key= c("incident_id","pct_id")) %>% 
  full_join(cc, key= c("incident_id","pct_id")) %>% 
  full_join(dd, key= c("incident_id","pct_id")) %>% 
  full_join(ee, key= c("incident_id","pct_id")) %>% arrange(desc(incident_id))





