library(nycflights13)
library(dplyr)

## Flitering joins

### 1. *semi_join(x,y, by="key")* keeps all the observations of x that have a match in y

#use semi-join() to collect the artists in the dataset *band* that have instrument info in the dataset *instrument*

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

semi_join(band, instrument)
