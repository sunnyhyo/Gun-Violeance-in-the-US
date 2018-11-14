#############
# title : gun violence 전처리
# author : 효선

# 패키지 설치
#install.packages("splitstackshape")
library(splitstackshape)
library(tidyverse)


# 경로 설정
setwd("C:/Users/HS/Documents/GitHub/Gun-violence-in-the-US")


# 원본 데이터 불러오기
# gun.data 는 원본이므로 건드리지 말기
gun.data <- read_csv("data/gun-violence-data.csv")
gun.data <- gun.data[,c(1:29)]
names(gun.data)



####################
# PREPARE DATASETS

# DATA1 : main dataset
del_variance <- c("address", "location_description", "notes", "sources" )
main.data <- gun.data %>% 
  select( -contains("url"), -starts_with("gun_"), -starts_with("participant"), -one_of(del_variance)) %>% 
  separate(date, c("year", "month", "day"), sep = "-") %>% 
  mutate(weekdays = weekdays(as.Date(gun.data$date), abbreviate = TRUE)) %>%   #우선 16개 완료
  filter(year == 2015 | year == 2016 | year == 2017 )

#write.csv(main.data, "data/data1_main.csv", row.names = FALSE)

#dim(gun.data)
#dim(main.data)
#names(main.data)
#head(main.data)


###############
# 전처리 필요한 변수 , || 분할
#onlygun.data <- gun.data %>% select(incident_id, starts_with("gun_"))
participant.data <- gun.data %>% 
  select(incident_id, starts_with("participant"),-participant_relationship, -participant_name) %>% 
  filter_all()

character.data <- gun.data %>% 
  select(incident_id, state, city_or_county, incident_characteristics)


###########
# DATA4 : character
# replacing "||" with "|" as both separators are used
character.data$incident_characteristics <- gsub("\\|\\|", "|", gun.data$incident_characteristics)
df4_charter <- cSplit(character.data, 'incident_characteristics', sep =  '|', direction="long")
head(df4_charter)
numCat <- round(nrow(df4_charter)/nrow(gun.data),1)
cat('On average, there are', numCat, 'incident categories specified per incident')

#write.csv(df4_charter, "data/data4_character.csv", row.names = FALSE)
#head(character.data$incident_characteristics)
#head(gun.data$incident_characteristics)


############
onlygun.data$gun_stolen <- gsub("\\|\\|", "|", gun.data$gun_stolen)
onlygun.data$gun_type <- gsub("\\|\\|", "|", gun.data$gun_type)
a <- head(cSplit(onlygun.data, c("gun_stolen"), sep = c("|"), direction = "long"))
b <- cSplit(a, "gun_stolen", sep = "::")
c <- cSplit(b, "gun_type", sep = "|", direction = "long") 
d <- cSplit(c, "gun_type", sep = "::")

###########
names(participant.data)
participant.data 
participant.data$participant_age <- gsub("\\|\\|", "|", gun.data$participant_age)
participant.data$participant_age_group <- gsub("\\|\\|", "|", gun.data$participant_age_group)
participant.data$participant_gender <- gsub("\\|\\|", "|", gun.data$participant_gender)
participant.data$participant_status <- gsub("\\|\\|", "|", gun.data$participant_status)
participant.data$participant_type <- gsub("\\|\\|", "|", gun.data$participant_type)

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





