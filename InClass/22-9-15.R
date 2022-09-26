a <- seq(1:20)
a[1:5]
b <- sample(1:20, 20)
b
b[b%%2==0]
#1. Print ONLY the odd numbers from a list of 50 random numbers ranging from 
#1 to 100. 
c <- sample(1:100, 50)
c[c%%2==1]
#2. state.name prints the names of all 50 states in the USA. How many states
#begin with an "N"? Print the names of those states only.
state.name
state.name[substr(state.name, 1, 1)=="N"]
#3. Create a subset of movies that were released in ONLY the even years.
movies <- read.csv("MOVIES.csv")
movies_even <- subset(movies, year%%2==0)
#4. What's the average $score of movies for EACH year? Which year shows the
#HIGHEST average score?
tapply(movies$score, movies$year, mean)

movies_rated <- subset(movies, rating%in%c("G", "PG", "PG-13", "R"))

#5. Create a new variable called movies_rated$rating2 by reassigning the 
#levels of $rating into either "R" or "Not R".
movies_rated$rating2 <- factor(movies_rated$rating, levels = c("R", "Not R"))
movies_rated$rating2[movies_rated$rating == "R"] <- "R"
movies_rated$rating2[movies_rated$rating == "PG-13"] <- "Not R"
movies_rated$rating2[movies_rated$rating == "PG"] <- "Not R"
movies_rated$rating2[movies_rated$rating == "G"] <- "Not R"

#6. Which $star appeared in the most number of movies? How well did those
#movies do in terms of both $gross and $score (i.e., report the average $gross
#and $score)? Try answering this question WITHOUT creating a subset.
which.max(table(movies_rated$star))
mean(movies_rated$gross[movies_rated$star == "Nicolas Cage"])
mean(movies_rated$score[movies_rated$star == "Nicolas Cage"])

gss <- read.csv("gss.csv")
sum(is.na(gss$age))
vh <- subset(gss, happy%in%"VERY HAPPY")
mean(vh$age)
summary(vh$age)
gss$marital2 <- factor(gss$marital, levels = c("MARRIED", "NOT MARRIED"))
gss$marital2[gss$marital != "MARRIED"] <- "NOT MARRIED"
mean(gss$age[gss$happy=="VERY HAPPY"], na.rm = TRUE)
