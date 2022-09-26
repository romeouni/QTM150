# Import the dataset and assign it into an object called movies. 
movies <- read.csv("MOVIES.csv")
str(movies)

# Print the first 10 rows of the movies dataset.
head(movies, 10)

# Without using the subset() function, create a new subset of movies called 
# movies2 by only including the following variables: "genre", "gross", "name",
# "rating", "score"
movies2 <- movies[c("genre", "gross", "name", "rating", "score")]

# Using subset() function, select only the movies that are rated G, PG, PG-13,
# or R and assign them into a new object called movies_rated.
movies_rated <- subset(movies, rating%in%c("G", "PG", "PG-13", "R"))

#quiz question
Comdy_USA <- subset(movies, genre=="Comedy"&country=="USA")

# $score variable shows the IMDB rating of the movies. Generally, movies with
# an IMDB score of 7 or higher are thought to be "good" movies. How many movies
# in movies_rated receive a score of 7 or better?
sum(movies_rated$score>7)

# The top 25% of the movies in the movies dataset shows that they grossed at
# least $40 million or more. summary(movies$gross)
# What's the average $score for these movies?
summary(movies_rated$gross)
topGross <- subset(movies_rated, gross>40000000)
mean(topGross$score)

# On average, which rating, PG-13 or R, receives higher $score and $gross?
mean(subset(movies_rated, rating=="PG-13")$score)
mean(subset(movies_rated, rating=="PG-13")$gross)
mean(subset(movies_rated, rating=="R")$score)
mean(subset(movies_rated, rating=="R")$gross)

# What is the average $gross of the movies that scored 7 or better? 
mean(subset(movies_rated, score>=7)$gross)

# What is the average $score of the movies that grossed $40 million or more?
mean(subset(movies_rated, gross>=40000000)$score)

# Create a new subset called, movies3, which contains only the movies that have
# scored 7 or better ($score) AND grossed $40 million or more ($gross).
movies3 <- subset(movies_rated, gross>=40000000 & score>=7)

# Which $director and $star are responsible for the most number of these
# "successful" movies?
names(which.max(table(movies3$director)))

# Finally, using movies3, produce an appropriate plot showing how well $score
# of movies predicts $gross of the movies. Describe a relationship between the
# two variables in plain English.

boxplot(movies3$gross~movies3$score)
#There's a  low correlation between score and gross.