library(plyr)

df2 <- read_csv("data/DATA2_participant2.csv")



names(df2)
head(df2)

df2$age <- as.integer(df2$age)
df2$age_group <- as.factor(df2$age_group)
df2$gender <- as.factor(df2$gender)
df2$status <- as.factor(df2$status)
df2$type <- as.factor(df2$type)
df2


df2$age_group <- revalue(df2$age_group, replace = c("1" = "Adult", "2" = "Child", "6" = "Teen"))
df2$gender <- revalue(df2$gender, replace = c("6" = "F", "16" = "M"))
df2$status <- revalue(df2$status, replace = c("1" = "Arrested",
                                              "2" = "Injured",
                                              "3" = "Injured, Arrested",
                                              "4" = "Injured, Unharmed",
                                              "5" = "Injured, Unharmed, Arrested",
                                              "6" = "Killed",
                                              "7" = "Killed, Arrested",
                                              "8" = "Killed, Injured",
                                              "9" = "Killed, Unharmed",
                                              "10" = "Killed, Unharmed, Arrested",
                                              "12" = "Unharmed",
                                              "13" = "Unharmed, Arrested"))
df2$type <- revalue(df2$type, replace = c("1" = "Subject-Suspect",
                                          "3" = "Victim"))
dim(df2)

write.csv(df2, "data/DATA2_participant3.csv", row.names = FALSE)
