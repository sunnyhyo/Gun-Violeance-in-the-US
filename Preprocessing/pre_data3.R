#DATA2, DATA3

library(tidyverse)
library(dplyr)

setwd("C:/Users/HS/Documents/GitHub/Gun-violence-in-the-US")
select <- dplyr::select

df2.data <- read_csv("data/DATA2_participant3.csv")
prac.data <- read_csv("data/practice.csv")
df2 <- df2.data
prac<- prac.data


df2 %>% count(status)
prac %>% count(status)
df2 %>% group_by(incident_id) %>% mutate(victim_=sum(type=="victim"))
df2 %>% group_by(incident_id) %>% count(pct_id)                                  
df2 %>% group_by(incident_id) %>% summarise(n_participant = last(pct_id)+1)
# n_participant
df2 %>% group_by(incident_id) %>% 
  summarise(victim_n = sum(type=="victim"), 
            suspect_n = sum(type=="Subject-Suspect"))
# n_victim, n_suspect
df2 %>% select(status, contains("killed")) %>%
  select(type, contains("Victim"))
df2 %>% select(status, contains("killed"), everything())


df2 %>% group_by(incident_id) %>%
  filter(type == Victim) %>%
  count(select(status, contains("killed")))

prac %>% mutate(new=prac$status[grep("Arrested",prac$status)])

prac11 = prac %>% 
  mutate(Arrested = status %in% c("Arrested", "Unharmed, Arrested", "Injured, Arrested", "Killed, Arrested")) %>% 
  mutate(status = str_replace(status, "Unharmed, Arrested", "Unharmed"),
         status = str_replace(status, "Injured, Arrested", "Injured"),
         status = str_replace(status, "Killed, Arrested", "Killed"),
         status = ifelse(prac$status == "Arrested", NA, status)) %>%
  mutate(Arrested = str_replace(Arrested, "FALSE", "Arrested_FALSE"), 
         Arrested = str_replace(Arrested, "TRUE", "Arrested_TRUE"))


prac11 %>% filter(status == "Arrested")
table(prac11$Arrested)
table(prac11$status)
table(prac11$type)


names(df2)
names(prac11)



df22 <- df2[,1:5]

df <- left_join(df22, prac11)
table(df$Arrested)
df
names(df)


table(df$age_group)
table(df$gender)
table(df$status)
table(df$type)

df2.data <- df %>% filter(!is.na(pct_id))
df2 <- df2.data

write.csv(df2, "C:/Users/HS/Documents/GitHub/Gun-Violeance-in-the-US/data/DATA2.csv", row.names = FALSE)



#DATA3

# part1 박효선
# pct_id
# n_participant
part1.1 <- df2 %>% 
  group_by(incident_id) %>% 
  summarise(n_participant = last(pct_id) + 1) 

#3
# age_group
# n_Adult, n_Child, n_Teen, n_NA_age_group
part1.2 <- df2 %>%   group_by(incident_id) %>% count(age_group) %>% 
  spread(key = age_group, value = n) %>% 
  dplyr::rename(n_Adult = Adult,
         n_Child = Child,
         n_Teen = Teen,
         n_NA_age_group = `<NA>`) 

# gender
# n_Female, n_Male, n_NA_gender
part1.3 <- df2 %>% group_by(incident_id) %>% count(gender) %>% 
  spread(key = gender, value = n) %>% 
  dplyr::rename(n_Female = Female,
                n_Male = Male,
                n_NA_gender = `<NA>`)
# status
# n_Injured, n_Killed, n_Unharmed, n_NA_status
part1.4 <- df2 %>% group_by(incident_id) %>% count(status) %>% 
  spread(key = status, value = n) %>% 
  dplyr::rename(n_Injured = Injured,
                n_Killed = Killed,
                n_Unharmed = Unharmed,
                n_NA_status = `<NA>`)

# type
# n_Suspect, n_Victim, n_NA_status
part1.5 <- df2 %>% group_by(incident_id) %>% count(type) %>% 
  spread(key = type, value = n) %>% 
  dplyr::rename(n_Suspect = `Subject-Suspect`,
                n_Victim = `Victim`,
                n_NA_type = `<NA>`)

# Arrested
# n_Arrest_FALSE, n_Arrest_TRUE
part1.6 <- df2 %>% group_by(incident_id) %>% count(Arrested) %>% 
  spread(key = Arrested, value = n) %>% 
  dplyr::rename(n_Arrest_FALSE = Arrested_FALSE,
                n_Arrest_TRUE = Arrested_TRUE)



# part2 국승지
# type_status
part2.1 <- df2 %>% 
  unite(y, status, type) %>% 
  mutate(y = str_replace(y, "Killed_Victim", "1"))%>%
  mutate(aa = (y == 1)) %>% 
  count(incident_id, wt = aa) %>%
  rename(n_Victim_Killed = n)

part2.2 <- df2 %>% unite(y, status, type) %>%
  mutate(y = str_replace(y, "Injured_Victim", "1")) %>%
  mutate(aa = (y == 1)) %>% 
  count(incident_id, wt = aa) %>% 
  rename(n_Victim_Injured = n)


part2.3 <- df2 %>% unite(y, status, type) %>% 
  mutate(y = str_replace(y, "Unharmed_Victim", "1")) %>% 
  mutate(aa = (y == 1)) %>%
  count(incident_id, wt=aa) %>%
  rename(n_Victim_Unharmed = n)


part2.4 <- df2 %>% unite(y, status, type) %>% 
  mutate(y = str_replace(y, "Killed_Subject-Suspect", "1")) %>% 
  mutate(aa = (y == 1)) %>%
  count(incident_id, wt = aa) %>% 
  rename(n_Suspect_Killed = n)

part2.5 <- df2 %>% unite(y, status, type) %>% 
  mutate(y = str_replace(y, "Injured_Subject-Suspect", "1")) %>% 
  mutate(aa = (y == 1)) %>% 
  count(incident_id, wt = aa) %>% 
  rename(n_Suspect_Injured = n)

part2.6 <- df2 %>% unite(y, status, type) %>% 
  mutate(y = str_replace(y, "Unharmed_Subject-Suspect", "1")) %>% 
  mutate(aa = (y == 1)) %>%
  count(incident_id, wt = aa) %>% 
  rename(n_Suspect_Unharmed = n)




# join

DATA3.part1 <- part1.1 %>% 
  left_join(part1.2 , by = "incident_id") %>%
  left_join(part1.3, by = "incident_id") %>% 
  left_join(part1.4, by = "incident_id") %>% 
  left_join(part1.5, by = "incident_id") %>%
  left_join(part1.6,  by = "incident_id")


DATA3.part2 <- part2.1 %>% 
  left_join(part2.2, by="incident_id") %>% 
  left_join(part2.3, by="incident_id") %>% 
  left_join(part2.4, by="incident_id") %>% 
  left_join(part2.5, by="incident_id") %>% 
  left_join(part2.6, by="incident_id")

DATA3.data <- left_join(DATA3.part1, DATA3.part2, by = "incident_id")

DATA3 <- DATA3.data

head(DATA3)
names(DATA3)
getwd()
write.csv(DATA3, "C:/Users/HS/Documents/GitHub/Gun-Violeance-in-the-US/data/DATA3.csv", row.names = FALSE)


#DATA1

df111 <- read_csv("C:/Users/HS/Documents/GitHub/Gun-violence-in-the-US/data/DATA1_main.csv")
df111 <- df111 %>% arrange(incident_id)
write.csv(df111, "C:/Users/HS/Documents/GitHub/Gun-Violeance-in-the-US/data/DATA1.csv", row.names = FALSE)



#DATA4

df444 <- read_csv("C:/Users/HS/Documents/GitHub/Gun-violence-in-the-US/data/DATA4_character.csv")
df444 <- df444 %>% arrange(incident_id)
write.csv(df444, "C:/Users/HS/Documents/GitHub/Gun-Violeance-in-the-US/data/DATA4.csv", row.names = FALSE)
