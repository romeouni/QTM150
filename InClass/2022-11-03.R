library(tidyverse)
library(lubridate)
library(nycflights13)
today()
now()

str(flights)

flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure_sched = make_datetime(year, month, day, hour, minute))

flights %>% 
  select(year, month, day, dep_time) %>% 
  separate(dep_time, into=c("act_h", "act_m"), sep=-2) %>% 
  mutate(departure_time = make_datetime(year, month, day, act_h, act_m))

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>%  # remove all cancelled fights
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt %>% 
  ggplot(aes(dep_time)) +
  geom_histogram()

flights_dt %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 86400)

flights_dt %>% 
  filter(dep_time < ymd("20130102")) %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 600)

str(flights_dt)

flights_dt %>% 
  mutate(day = wday(dep_time, label=T)) %>% 
  ggplot(aes(day)) +
  geom_bar()

flights_dt %>% 
  mutate(minute = minute(sched_dep_time))
