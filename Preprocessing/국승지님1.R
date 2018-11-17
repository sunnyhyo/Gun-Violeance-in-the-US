
library(tidyverse)

setwd("C:/Users/HS/Documents/GitHub/Gun-violence-in-the-US")
prac1 <- read_csv("data/practice.csv")

b = prac1 %>% group_by(incident_id) %>%
  mutate(Arrested = sum(Arrested == "TRUE")) %>%
  count(incident_id, wt = Arrested) %>% 
  rename(arrest = n)

c = prac1 %>% unite(y,status,type) %>%
  mutate(y=str_replace(y,"Killed_Victim","1"))%>%
  mutate(aa=(y==1)) %>% 
  count(incident_id,wt=aa) %>% 
  rename(n_victim_killed=n)

d = prac1 %>% unite(y,status,type) %>%
  mutate(y=str_replace(y,"Injured_Victim","1"))%>%
  mutate(aa=(y==1)) %>% 
  count(incident_id,wt=aa) %>% 
  rename(n_victim_injured=n)

e = prac1 %>% unite(y,status,type) %>% 
  mutate(y=str_replace(y,"Unharmed_Subject-Suspect","1")) %>% 
  mutate(aa=(y==1)) %>% 
  count(incident_id,wt=aa) %>% 
  rename(n_suspect_unharmed=n)

f = prac1 %>% unite(y,status,type) %>% 
  mutate(y=str_replace(y,"Unharmed_Victim","1")) %>% 
  mutate(aa=(y==1)) %>% 
  count(incident_id,wt=aa) %>% 
  rename(n_victim_unharmed=n)

g = prac1 %>% unite(y,status,type) %>% 
  mutate(y=str_replace(y,"Killed_Subject-Suspect","1")) %>% 
  mutate(aa=(y==1)) %>% 
  count(incident_id,wt=aa) %>% 
  rename(n_suspect_killed=n)

h = prac1 %>% unite(y,status,type) %>%
  mutate(y=str_replace(y,"Injured_Subject-Suspect","1")) %>% 
  mutate(aa=(y==1)) %>%
  count(incident_id,wt=aa) %>% 
  rename(n_suspect_injured=n)

DATA3 = df2 %>% 
  full_join(b,by="incident_id") %>% 
  full_join(c,by="incident_id") %>% 
  full_join(d,by="incident_id") %>% 
  full_join(e,by="incident_id") %>% 
  full_join(f,by="incident_id") %>% 
  full_join(g,by="incident_id") %>% 
  full_join(h,by="incident_id") 