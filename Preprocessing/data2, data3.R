library(tidyverse)
library(nycflights13)
library(ggplot2)

weather %>% count(year, month, day, hour, origin) %>% 
  filter( n > 1 )

# 탐색적 자료 분석(EDA)

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))
diamonds %>% count(cut)

smaller <- diamonds %>% filter( carat > 30)

ggplot(smaller) + geom_histogram(mapping = aes(x = carat, color = cut))

ggplot(diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5)

ggplot(diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5) + coord_cartesian()

unusual <- diamonds %>% filter( y <3 | y >20) %>% arrange(y)
unusual

nycgflights13::flights %>% mutate(
  cancelles = is.na(),
  
)