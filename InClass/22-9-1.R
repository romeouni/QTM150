getwd()
usArrests_csv <- read.csv("USArrests.csv")
dim(usArrests_csv)
str(usArrests_csv)
usArrests_csv$State
usArrests_csv$Murder
mean(usArrests_csv$Murder)
summary(usArrests_csv$Murder)
summary(usArrests_csv)

