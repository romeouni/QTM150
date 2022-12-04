library(anyflights)
library(tidyverse)


airports <- get_airports()
airlines <- get_airlines()


may2019 <- read.csv("flights_may2019.csv")
may2020 <- read.csv("flights_may2020.csv")
may2021 <- read.csv("flights_may2021.csv")

may2019 %>% 
  select(carrier) %>% 
  count(carrier) %>% 
  arrange(desc(n))

sel_airlines <- c("DL", "UA", "AA", "NK", "B6", "HA")

may2019mod <- may2019 %>%
  filter(carrier %in% sel_airlines) %>% 
  select(year, month, day, carrier, origin, dest, time_hour)

may2020mod <- may2020 %>% 
  filter(carrier %in% sel_airlines) %>% 
  select(year, month, day, carrier, origin, dest, time_hour)

may2021mod <- may2021 %>% 
  filter(carrier %in% sel_airlines) %>% 
  select(year, month, day, carrier, origin, dest, time_hour)

ex <- may2019mod %>% 
  group_by(carrier, dest) %>% 
  mutate(count = n()) %>% 
  summarize(carrier, dest, count)

ex2 <- ex[!duplicated(ex),]  

ex3 <- ex2 %>% 
  pivot_wider(names_from="dest",values_from="count")

sel_airports = c("SEA", "IAH", "ATL", "JFK", "MCO", "ORD")  

final2019 <- ex3 %>% 
  select(carrier, sel_airports)
###############################################################################

ex <- may2020mod %>% 
  group_by(carrier, dest) %>% 
  mutate(count = n()) %>% 
  summarize(carrier, dest, count)

ex2 <- ex[!duplicated(ex),]  

ex3 <- ex2 %>% 
  pivot_wider(names_from="dest",values_from="count")

final2020 <- ex3 %>% 
  select(carrier, sel_airports)
###############################################################################

ex <- may2021mod %>% 
  group_by(carrier, dest) %>% 
  mutate(count = n()) %>% 
  summarize(carrier, dest, count)

ex2 <- ex[!duplicated(ex),]  

ex3 <- ex2 %>% 
  pivot_wider(names_from="dest",values_from="count")

final2021 <- ex3 %>% 
  select(carrier, sel_airports)