gss <- read.csv("gss.csv")
#1.
sum(is.na(gss$degree))
#142 observations are missing for the $degree variable.
#2.
100*sum(gss$sex %in% "MALE" & gss$marital %in% "MARRIED")/sum(gss$sex %in% "MALE")
#58.06% of male respondents are married.
100*sum(gss$sex %in% "FEMALE" & gss$marital %in% "MARRIED")/sum(gss$sex %in% "FEMALE")
#50.24% of female respondents are married.
#3.
100*sum(gss$sex %in% "MALE" & gss$marital %in% "MARRIED")/sum(gss$marital %in% "MARRIED")
#47.44% of married respondents are male.
100*sum(gss$sex %in% "FEMALE" & gss$marital %in% "MARRIED")/sum(gss$marital %in% "MARRIED")
#52.56% of married respondents are female.
#4.
barplot(table(gss$degree))
#The x axis is not organised by any logical metric, it's alphabetical. The most
#common degree is High School.
#5.
sum(gss$sex %in% "FEMALE" & (gss$degree %in% "JUNIOR COLLEGE" | gss$degree %in% "BACHELOR" | gss$degree %in% "GRADUATE"))
#7294 female respondents completed more than high school.
#6.
sum(gss$sex %in% "MALE" & (gss$degree %in% "LT HIGH SCHOOL" | gss$degree %in% "HIGH SCHOOL"))
#16648 male respondents completed high school or less.
#7.
table(gss$year)
#2006 had the most respondents, and 1990 had the least.
#8.
gss$degree3 <- factor(gss$degree, levels = c("<HS", "HIGH SCHOOL", ">HS"), ordered = TRUE)
gss$degree3[gss$degree=="LT HIGH SCHOOL"] <- "<HS"
gss$degree3[gss$degree=="BACHELOR"] <- ">HS"
gss$degree3[gss$degree=="JUNIOR COLLEGE"] <- ">HS"
gss$degree3[gss$degree=="GRADUATE"] <- ">HS"
mean(gss$age[gss$degree3=="<HS"], na.rm=TRUE)
mean(gss$age[gss$degree3=="HIGH SCHOOL"], na.rm=TRUE)
mean(gss$age[gss$degree3==">HS"], na.rm=TRUE)
#The average age for <HS is 52.94, the average age for HIGH SCHOOL is 43.42,
#the average age for >HS is 43.84.
#9.
gss$happy2 <- factor(ifelse(gss$happy=="VERY HAPPY", "HAPPY", "NOT HAPPY"))
gss$happy2[is.na(gss$happy)] <- "NOT HAPPY"
#10.
gss$decade <- factor(gss$year, levels = c("1970s", "1980s", "1990s", "2000s"), ordered = TRUE)
gss$decade[gss$year>=1973 & gss$year<=1980] <- "1970s"
gss$decade[gss$year>=1981 & gss$year<=1990] <- "1980s"
gss$decade[gss$year>=1991 & gss$year<=2000] <- "1990s"
gss$decade[gss$year>=2001 & gss$year<=2010] <- "2000s"
#11.
100*sum(gss$happy2 %in% "HAPPY" & gss$decade %in% "1970s")/sum(gss$decade %in% "1970s")
#34.71% of respondents in the 1970s were happy.
100*sum(gss$happy2 %in% "HAPPY" & gss$decade %in% "1980s")/sum(gss$decade %in% "1980s")
#31.35% of respondents in the 1980s were happy.
100*sum(gss$happy2 %in% "HAPPY" & gss$decade %in% "1990s")/sum(gss$decade %in% "1990s")
#30.54% of respondents in the 1990s were happy.
100*sum(gss$happy2 %in% "HAPPY" & gss$decade %in% "2000s")/sum(gss$decade %in% "2000s")
#20.43% of respondents in the 2000s were happy.
#12.
gss$income_ordered <- factor(ifelse(gss$income<10000, "Less than 10k",
                             ifelse(gss$income>=10000&gss$income<=25000, "Between 10k and 25k",
                             "More than 25k")))






