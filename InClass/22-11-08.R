library(tidyverse)
library(dplyr)
library(ggplot2)
library(nycflights13)
library(forcats)
library(lubridate)
library(tidyr)

#################################### QUIZ 7 ####################################
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

gss_cat %>%
  drop_na(tvhours) %>%
  mutate(partyidnew = fct_recode(partyid,
                                 "Republican, strong"    = "Strong republican",
                                 "Republican, weak"      = "Not str republican",
                                 "Independent, near rep" = "Ind,near rep",
                                 "Independent, near dem" = "Ind,near dem",
                                 "Democrat, weak"        = "Not str democrat",
                                 "Democrat, strong"      = "Strong democrat",
                                 "Other"                 = "No answer",
                                 "Other"                 = "Don't know",
                                 "Other"                 = "Other party")) %>% 
  group_by(partyidnew) %>% 
  summarize(avg_age=mean(age, na.rm=T)) %>% 
  ggplot(aes(x=avg_age, y=fct_reorder(partyidnew, avg_age))) +
  geom_col()

str(flights_dt)

flights_dt %>% 
  mutate(weekday = wday(sched_arr_time), label=T) %>% 
  group_by(weekday) %>% 
  summarize(avg_arr_delay = mean(arr_delay, na.rm=T))   %>% 
  ggplot(aes(x=weekday, y=avg_arr_delay)) +
  geom_col()
###############################################################################

gss_cat %>% 
  mutate(racenew = ifelse(race %in% "White", 1, 0))




