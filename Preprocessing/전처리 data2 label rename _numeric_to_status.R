library(plyr)

df2.1 <- read_csv("DATA2_participant2.csv")



names(df2.1)
head(df2.1)

df2.1$age <- as.integer(df2.1$age)
df2.1$age_group <- as.factor(df2.1$age_group)
df2.1$gender <- as.factor(df2.1$gender)
df2.1$status <- as.factor(df2.1$status)
df2.1$type <- as.factor(df2.1$type)
df2.1


df2.1$age_group <- revalue(df2.1$age_group, replace = c("1" = "Adult",   #Adult
                                                    "2" = "Child",   #Child 
                                                    "3" = "Teen"))  #Teen
df2.1$gender <- revalue(df2.1$gender, replace = c("1" = "Female",   #Female
                                              "2" = "Male")) #Male
df2.1$status <- revalue(df2.1$status, replace = c("1" = "Arrested",
                                              "2" = "Injured",
                                              "3" = "Injured, Arrested",
                                              "4" = "Injured",
                                              "5" = "Injured, Arrested",
                                              "6" = "Killed",
                                              "7" = "Killed, Arrested",
                                              "8" = "Killed",
                                              "9" = "Killed",
                                              "10" = "Killed, Arrested",
                                              "12" = "Unharmed",
                                              "13" = "Unharmed, Arrested"))
df2.1$type <- revalue(df2.1$type, replace = c("1" = "Suspect", #Subject-Suspect
                                          "2" = "Victim")) #Victim
dim(df2.1)

table(df2.1$status)


n <- df2.1[,c(1,2,6,7)]
getwd()
write.csv(df2.1, "practice1.csv", row.names = FALSE)

