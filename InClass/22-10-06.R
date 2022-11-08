library(dplyr)
library(ggplot2)
library(ggthemes)
setwd("~/Downloads")
ATL2020 <- read.csv("ATL2020.csv", stringsAsFactors = TRUE)
setwd("~/github/QTM150/InClass")
str(ATL2020)
table(ATL2020$month)
barplot(table(ATL2020$month))
ATLcarrier <- prop.table(table(ATL2020$carrier))
sort(ATLcarrier, decreasing = TRUE)


ggplot(ATL2020, aes(carrier))+
  geom_bar()+
  facet_wrap(~dest)

ATL2020 %>% 
  group_by(dest) %>% 
  summarize(avg_dist = mean(distance, na.rm = TRUE)) %>% 
  arrange(desc(avg_dist))

ATL2020 %>% 
  filter(carrier=="AA") %>% 
  group_by(dest) %>% 
  summarize(avg_dist = mean(distance, na.rm = TRUE),
            avg_airtime = mean(air_time, na.rm = TRUE)) %>% 
  arrange(avg_dist)

ATL2020 %>% 
  filter(carrier=="DL") %>% 
  group_by(dest) %>% 
  summarize(avg_dist = mean(distance, na.rm = TRUE),
            avg_airtime = mean(air_time, na.rm = TRUE)) %>% 
  arrange(avg_dist)

#subset of morning flights to LAX with no delay during first half of 2020
ATL_LAX_H1 <- filter(ATL2020, month<=6,
                     dest=="LAX",
                     dep_time>=600 & dep_time<=1159,
                     arr_delay<=0)
summary(ATL_LAX_H1)

ATL_LAX_H1 %>% 
  group_by(carrier) %>% 
  summarize(avg_airtime = mean(air_time/60)) %>% 
  arrange(avg_airtime)

#print only the airline with the shortest airtime on average
airtime_carrier <- ATL_LAX_H1 %>% 
  group_by(carrier) %>% 
  summarize(avg_airtime = mean(air_time/60)) %>% 
  arrange(avg_airtime)
airtime_carrier[1,]

#OR

ATL_LAX_H1 %>% 
  group_by(carrier) %>% 
  summarize(avg_airtime = mean(air_time)/60) %>% 
  summarize(carrier = carrier[which.min(avg_airtime)],
            shortest_time = min(avg_airtime))
