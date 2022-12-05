library(anyflights)
library(tidyverse)
library(plotly)
library(ggmap)
library(maps)

airports <- get_airports()
airlines <- get_airlines()


may2019 <- read.csv("flights_may2019.csv")
may2020 <- read.csv("flights_may2020.csv")
may2021 <- read.csv("flights_may2021.csv")

may2019 %>% 
  select(carrier) %>% 
  count(carrier) %>% 
  arrange(desc(n))

sel_airlines <- c("DL", "UA", "AA", "B6")

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

sel_airports = c("SEA", "LAS", "SAT", "MCO", "TPA")  

final2019 <- ex3 %>% 
  select(carrier, sel_airports) %>% 
  mutate(year = 2019)
###############################################################################

ex <- may2020mod %>% 
  group_by(carrier, dest) %>% 
  mutate(count = n()) %>% 
  summarize(carrier, dest, count)

ex2 <- ex[!duplicated(ex),]  

ex3 <- ex2 %>% 
  pivot_wider(names_from="dest",values_from="count")

final2020 <- ex3 %>% 
  select(carrier, SEA, LAS, MCO)

final2020 <- final2020 %>% 
  mutate(SAT = 0,
         TPA = 0,
         year = 2020)
###############################################################################

ex <- may2021mod %>% 
  group_by(carrier, dest) %>% 
  mutate(count = n()) %>% 
  summarize(carrier, dest, count)

ex2 <- ex[!duplicated(ex),]  

ex3 <- ex2 %>% 
  pivot_wider(names_from="dest",values_from="count")

final2021 <- ex3 %>% 
  select(carrier, sel_airports) %>% 
  mutate(year = 2021)
###############################################################################


final2019 <- final2019 %>% 
  replace(is.na(.), 0)

final2020 <- final2020 %>% 
  replace(is.na(.), 0)

final2021 <- final2021 %>% 
  replace(is.na(.),0)


change2019_2020 <- left_join(final2019, final2020, by="carrier")

change2019_2020 <- change2019_2020 %>% 
  mutate(ChangeSEA = SEA.y - SEA.x,
         ChangeLAS = LAS.y - LAS.x,
         ChangeSAT = SAT.y - SAT.x,
         ChangeMCO = MCO.y - MCO.x,
         ChangeTPA = TPA.y - TPA.x) %>% 
  select(carrier, ChangeSEA, ChangeLAS, ChangeMCO, ChangeTPA) %>% 
  rename("SEA" = "ChangeSEA",
         "LAS" = "ChangeLAS",
         "MCO" = "ChangeMCO",
         "TPA" = "ChangeTPA")


newChange <- t(change2019_2020)

newChange <- as.data.frame(newChange)

colnames(newChange) <- newChange[1,]

newChange <- newChange[-1,]

newChange <- newChange %>% 
  mutate(faa = c("SEA", "LAS", "MCO", "TPA"))


rownames(newChange) <- c(1, 2, 3, 4)

airports <- airports %>% 
  filter(faa %in% sel_airports) %>% 
  select(faa, name, lat, lon)

newChange <- newChange %>% 
  left_join(airports, by="faa")

newChange <- newChange %>% 
  transform(AA = as.numeric(AA),
            B6 = as.numeric(B6),
            DL = as.numeric(DL),
            UA = as.numeric(UA))

us <- c(left = -125, bottom = 24, right = -67, top = 49)
map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")

AAplot <- ggmap(map) +
  geom_point(data = newChange, aes(x=lon, y=lat, color = AA, ids = name)) +
  scale_color_gradient(low = "red", high="green")

ggplotly(AAplot)
