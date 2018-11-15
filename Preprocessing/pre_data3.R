library(tidyverse)
count <- dplyr::count

setwd("C:/Users/HS/Documents/GitHub/Gun-violence-in-the-US/data")

df2 <- read_csv("DATA2_participant3.csv")
df2$incident_id <- as.factor(df2$incident_id)
# df2$pct_id <- as.numeric(df2$pct_id)


df %>% count(incident_id) %>% filter(n>1)

names(df2)
#2
df2 %>% group_by(incident_id) %>% summarise(n_participant = last(pct_id) + 1) #n_participant

#3
a <- df2 %>% group_by(incident_id) %>% count(type)
a %>% spread(key = type, value = n)

b <- df2 %>% group_by(incident_id) %>% count(age_group)
b %>% spread(key = age_group, value = n) %>% print(n =50)

c <- df2 %>% group_by(incident_id) %>% count(gender)
c %>% spread(key = gender, value = n)

df2

