library(tidyverse)
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

df2 %>% group_by(incident_id) %>% 
  summarise(victim_n = sum(type=="victim"), suspect_n = sum(type=="Subject-Suspect"))
#n_victim, n_suspect


df2 %>% select(status, contains("killed")) %>%
  select(type, contains("Victim"))

df2 %>% select(status, contains("killed"), everything())


df2 %>% group_by(incident_id) %>%
  filter(type == Victim) %>%
  count(select(status, contains("killed")))

prac %>% mutate(new=prac$status[grep("Arrested",prac$status)])

prac1 = prac %>% mutate(Arrested = status%in%c("Arrested","Unharmed, Arrested")) %>% 
  mutate(status=str_replace(status,"Unharmed, Arrested","Unharmed")) %>%
  mutate(Arrested=str_replace(Arrested,"FALSE","0"))
b=prac1 %>% 
  group_by(incident_id) %>%
  mutate(Arrested=sum(Arrested=="TRUE")) %>% 
  count(incident_id,wt=Arrested) %>% rename(arrest=n)


c=prac1 %>% 
  unite(y,status,type) %>% 
  mutate(y=str_replace(y,"Killed_Victim","1"))%>%
  mutate(aa=(y==1)) %>% 
  count(incident_id,wt=aa) %>%
  rename(n_victim_killed=n)

d=prac1 %>% unite(y,status,type) %>%
  mutate(y=str_replace(y,"Injured_Victim","1")) %>% mutate(aa=(y==1)) %>% 
  count(incident_id,wt=aa) %>% rename(n_victim_injured=n)

e=prac1 %>% unite(y,status,type) %>% 
  mutate(y=str_replace(y,"Unharmed_Subject-Suspect","1"))%>%mutate(aa=(y==1)) %>%
  count(incident_id,wt=aa) %>% rename(n_suspect_unharmed=n)
f=prac1 %>% unite(y,status,type) %>% 
  mutate(y=str_replace(y,"Unharmed_Victim","1")) %>% mutate(aa=(y==1)) %>%
  count(incident_id,wt=aa) %>% rename(n_victim_unharmed=n)
g=prac1 %>% unite(y,status,type) %>% 
  mutate(y=str_replace(y,"Killed_Subject-Suspect","1")) %>% mutate(aa=(y==1)) %>%
  count(incident_id,wt=aa) %>% rename(n_suspect_killed=n)
h=prac1 %>% unite(y,status,type) %>% mutate(y=str_replace(y,"Injured_Subject-Suspect","1")) %>% 
  mutate(aa=(y==1)) %>% count(incident_id,wt=aa) %>% rename(n_suspect_injured=n)
DATA3= df2 %>% full_join(b,by="incident_id") %>% full_join(c,by="incident_id") %>% full_join(d,by="incident_id")%>% full_join(e,by="incident_id")%>% full_join(f,by="incident_id")%>% full_join(g,by="incident_id") %>% 
  full_join(h,by="incident_id")


write.csv()
write.csv(df, "경로", row.names = FALSE)