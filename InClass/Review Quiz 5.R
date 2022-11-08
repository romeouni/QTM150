library(dplyr)
library(ggplot2)
ATL2020 <- read.csv("ATL2020.csv")
ATL2020 %>% 
  filter(carrier == "F9") %>% 
  group_by(dest) %>% 
  summarize(del = mean(dep_delay, na.rm = T)) %>% 
  arrange(desc(del))


ATL2020 %>% 
  filter(carrier == "WN",
         month <= 6) %>% 
  group_by(dest) %>% 
  summarize(dist = mean(distance, na.rm = T)) %>% 
  arrange(desc(dist)) %>% 
  ggplot(aes(carrier)) +
  geom_bar()



ATL2020 %>% 
  filter(carrier == "B6") %>% 
  group_by(dest) %>% 
  summarize(dist = mean(distance, na.rm = T),
            del = mean(arr_delay, na.rm = T)) %>% 
  arrange(desc(dist))
