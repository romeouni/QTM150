library(dplyr)
library(ggplot2)
library(nycflights13)


# tailnum is the foreign key in flights (uniquely identifies an observation in another table)
flights %>% 
  count(tailnum) %>% 
  filter(n>1) %>% 
  arrange(desc(n))


# examining weather dataset

str(weather)
summary(weather$year)
table(weather$origin)

# pring only the ones with more than 1 observation
weather %>% 
  count(year, month, day) %>% 
  filter(n>1)

#how many flights per each origin at the same hour?
flights %>% 
  count(year, month, day, hour) %>% 
  filter(n>1)

flights %>% 
  count(year, month, day, flight, sort=T) %>% 
  filter(n>1)

# a tibble/tribble is a trimmed down version of data.frame(), there are several good features about tribble,
# the function tribble() is used to create a small data set
band <- tribble(
  ~name, ~band,
  "Mick", "Stones",
  "John", "Beatles",
  "Paul", "Beatles"
)

band

instrument <- tribble(
  ~name, ~plays,
  "John", "guitar",
  "Paul", "bass",
  "Keith", "guitar"
)

instrument

# XXX_join(x, y, by="key variable")
# join y to x, x is the primary data set
# mutate new variables in x by copying different variables in y
# return a new data set

#first, inner_join() will join ONLY observations between two data sets that SHARE the key.
inner_join(band, instrument, by="name")
inner_join(instrument, band, by="name")

# ful_join joins ALL of the observations
full_join(band, instrument, by="name")

#left_join()
# joining band(primary) and instrument(secondary) by name. results in adding a new variable (plays) to band
left_join(band, instrument, by="name")


right_join(band, instrument, by="name")
