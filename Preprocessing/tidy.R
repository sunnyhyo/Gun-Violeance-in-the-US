#Tidy data 연습
#1104


#package
library(tidyverse)
library(ggplot2)

table1
table2
table4a #cases
table4b #population

# Tidy 자료 
# 각 변수는 각자의 column을 가지고 있어야 한다
# 각 observation은 각각의 row를 가지고 있어야 한다
# 각 값은 각자의 cell 을 가지고 있어야 한다

# Tidy 자료의 장점
# 자료를 저장하는데에 일관된 방법을 제공하고 이후 작업을 위한 도구들을 이에 맞춰 개발하므로 배우기 쉽다
# 변수를 column 으로 놓는 것은 특히 유용하다 
# R의 내장된 함수들은 vector를 기본으로 하고 있으므로 tidy data 를 vector 로 변환하는 것이 유용

# Compute rate per 10,000
table1 %>% 
  mutate(rate = cases / population *10000)

# Compute cases per year
table1 %>%  count(year) #3, 3
table1 %>% 
  count(year, wt = cases) # 250740, 296020

# Visualise changes over time
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

# Spreading and gathering
# gathering
table4a
# 
# - `1999` 와 `2000`는 변수의 이름이 아니고 year 변수의 값이 됨.  -  row 는 하나가 아니고 2 개의 관측을 나타낸다.  • tidy 자료로 만들기 위해서 column을 새로운 변수의 pair로 만들어야함. 이를 gathering이라고 함. • 이를 위하여 3가지의 인자를 지정하는 것이 필요. 1. 값을 나타내는 column의 이름; table4a 에서는 1999 와 2000 2. column의 이름에 나타난 값을 위한 변수 이름 (key); ‘year’ 3. column에서 가지고 있는 값을 저장하기 위한 변수 이름 (value);‘cases’ 
# - row는 하나가 아니고 2개의 관측을 나타낸다
# tidy 자료로 만들기 위해서 column을 새로운 변수의 pair 로 만들어야 한다
# 이를 gathering 이라고 함
# 1. 값을 나타내는 column 의 이름; table4a 에서는 1999와 2000
# 2. column 의 이름에 나타난 값을 위한 변수이름 (key); year
# 3. column 에서 가지고 있는 값을 저장하기 위한 변수 이름 (value); cases
# 3. column 에서 가지고 있는 값을 저장하기 위한 변수 이름 (value); population
table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")


# table4b 도 같은 형태로 변형 가능
table4b
table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")

# table4a 와 table4b를 tidy data 로 바꾼 후 dplyr 패키지의 left_join()을 이용하여
# 하나의 tibble 로 나타내기 (자세한 내용은 relational data 에서)

tidy4a <- table4a %>% 
  gather(`1999`,`2000`, key = "year", value = "cases")

tidy4b <- table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")

left_join(tidy4a, tidy4b)


######
# spreading 
# spreading 은 gathering 의 반대로 observation 이 여러줄에 나타낼 때 이용

table2

# 위의 자료를 tidy 자료로 바꾸기 위해서는 두가지의 인자가 필요
# 1. 변수 이름이 저장되어 있는 key column : 여기서는 type
# 2. 변수의 값이 저장되어 있는 value column : 여기서는 count

spread(table2, key = type, value = count)

#spreating and uniting











