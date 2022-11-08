library(ggplot2)
library(dplyr)
library(nycflights13)
library(tidyverse)

major <- tribble(
  ~student, ~major,
  "Kim", "CHEM",
  "Johnson", "BIOL",
  "Smith", "ENG",
  "Lee", "SOC"
)


minor <- tribble(
  ~student, ~minor,
  "Smith", "QSS",
  "Johnson", "PSYC",
  "Jones", "ENG"
)

combined <- left_join(minor, major, by="student")

combined2 <- right_join(minor, major, by="student")




table1<-tibble(
  `country`=c("Afghanistan","Brazil","China"),
  `1999`=c(19987071, 172006362, 1272915272),
  `2000`=c(20595360, 174504898, 1280428583)
)


table2<-tibble(
  `country` = c("Afghanistan","Brazil","China"),
  `1999` = c(745, 37737, 212258),
  `2000` = c(2666, 80488, 213766)
)


pivot_longer(table1, c("1999":"2000"), names_to = "year", values_to = "population")



table3<- tibble(
  `country` = c("Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan", "Brazil", "Brazil", "Brazil", "Brazil","China", "China","China", "China"),
  `year` = c(1999,1999,2000,2000,1999,1999,2000,2000,1999,1999,2000,2000),
  `type` = c("case", "population","case", "population","case", "population","case", "population","case", "population","case", "population"),
  `count` = c(745, 19987071, 2666, 20595360, 37737, 172006362, 80488, 174504898, 212258, 1272915272, 213766, 1280428583)
)

table3 %>%
  pivot_wider(names_from = "type", values_from = "count")

table4<-tibble(
  `country` = c("Afghanistan", "Afghanistan", "Brazil", "Brazil","China", "China"),
  `year` = c(1999,2000,1999,2000,1999,2000),
  `rate`= c("745/19987071", "2666/20595360", "37737/172006362", "80488/174504898", "212258/1272915272", "213766/1280428583")
)

separate(table4, rate, into=c("case","pupulation"), sep="/")

flights2 <- unite(flights, date, year, month, day)

flights3 <- flights %>%
  unite(date, year,month, day) %>%
  separate(time_hour, into=c("date1","hour1"), sep=10)
str(flights3)
