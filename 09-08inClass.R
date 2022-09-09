movies <- read.csv("MOVIES.csv")
head(movies, 10)
movies2 <- movies[c("genre", "gross", "name", "rating", "score")]
movies2
movies_rated <- subset(movies, rating=="G"|rating=="PG"|rating=="PG-13"|rating=="R")
sum(movies_rated$score>7)
summary(movies_rated$gross)
topGross <- subset(movies_rated, gross>40000000)
mean(topGross$score)
mean(subset(movies_rated, rating=="PG-13")$score)
mean(subset(movies_rated, rating=="PG-13")$gross)
mean(subset(movies_rated, rating=="R")$score)
mean(subset(movies_rated, rating=="R")$gross)
mean(subset(movies_rated, score>=7)$gross)
