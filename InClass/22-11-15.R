library(tidyverse)
data(mtcars)


avg <- numeric(ncol(mtcars))
for(i in 1:ncol(mtcars)){
  avg[i] <- mean(mtcars[,i])
}

median(avg)

mtcars <- mtcars %>% 
  mutate(hp_cat = NA)

for(j in 1:length(mtcars$hp)){
  if(mtcars$hp[j]<95){
    mtcars$hp_cat[j] = "slow"
  }
  else if(mtcars$hp[j]>=180){
    mtcars$hp_cat[j] = "fast"
  }
  else{
    mtcars$hp_cat[j] = "average"
  }
}

length(mtcars$hp)
