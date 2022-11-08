gss2k <- read.csv("gss2k.csv", stringsAsFactors = TRUE)
str(gss2k)
table(gss2k$year)
happy_tab <- table(gss2k$happy)
happy_tab
max(happy_tab)
which.max(happy_tab)
prop.table(happy_tab)
prop.table(happy_tab)*100
sex_by_happy_tab <- table(gss2k$sex, gss2k$happy)
sex_by_happy_tab
prop.table(sex_by_happy_tab)
prop.table(sex_by_happy_tab, margin = 1)
prop.table(sex_by_happy_tab, margin = 2)
med_age_sex <- tapply(gss2k$age, gss2k$sex, median, na.rm = TRUE)
med_age_sex
avg_age_year <- tapply(gss2k$age, gss2k$year, mean, na.rm = TRUE)
avg_age_year
max(avg_age_year)
min(avg_age_year)
which.max(avg_age_year)
which.min(avg_age_year)
is.numeric(gss2k$age)
summary(gss2k$age)
table(gss2k$age)
gss2k$age_factor_quick <- as.factor(gss2k$age)
is.factor(gss2k$age_factor_quick)
levels(gss2k$age_factor_quick)
gss2k$age_factor <- factor(gss2k$age, levels=c("young adult", "middle-aged", "older adult"), ordered = TRUE)
gss2k$age_factor[gss2k$age>=18 & gss2k$age<=35] <- "young adult"
gss2k$age_factor[1:100]
gss2k$age_factor[gss2k$age>35 & gss2k$age<=55] <- "middle-aged"
gss2k$age_factor[gss2k$age>55] <- "older adult"
summary(gss2k$age_factor)
barplot(table(gss2k$age_factor))
gss2k$age1<-factor(gss2k$age, levels=c("26.5","45.5","77.5"))
gss2k$age1[gss2k$age_factor=="young adult"]<-"26.5"
gss2k$age1[gss2k$age_factor=="middle-aged"]<-"45.5"
gss2k$age1[gss2k$age_factor=="older adult"]<-"77.5"
gss2k$age1 <- as.numeric(as.character(gss2k$age1))
is.numeric(gss2k$age1)
gss2k$age_num <- gss2k$age_factor
levels(gss2k$age_num) <- c(26.5, 45.5, 77.5)
is.numeric(gss2k$age_num)
gss2k$age_num <- as.numeric(as.character(gss2k$age_num))

##QUIZ 3

gss2k2 <- read.csv("gss2k.csv", stringsAsFactors = TRUE)
gss2k2$degree2 <- factor(gss2k2$degree, levels = c("LT HIGH SCHOOL", "HIGH SCHOOL", "JUNIOR COLLEGE", "BACHELOR", "GRADUATE"), ordered = TRUE)
levels(gss2k2$degree2)

gss2k$income2 <- factor(gss2k$income, levels = c("Less than $10k", "Between $10K and $20K", "More than $20K"), ordered = TRUE)
gss2k$income2[gss2k$income<10000] <- "Less than $10k"
summary(gss2k$income)
gss2k$income2[gss2k$income=="$1000 TO 2999" | 
                gss2k$income=="$3000 TO 3999" | 
                gss2k$income=="$4000 TO 4999" | 
                gss2k$income=="$5000 TO 5999" |
                gss2k$income=="$6000 TO 6999" |
                gss2k$income=="$7000 TO 7999" |
                gss2k$income=="$8000 TO 9999" |
                gss2k$income=="LT $1000"] <- "Less than $10k"
gss2k$income2[gss2k$income=="$10000 - 14999" |
              gss2k$income=="$15000 - 19999"] <- "Between $10K and $20K"
gss2k$income2[gss2k$income=="$20000 - 24999" |
                gss2k$income=="$25000 OR MORE"] <- "More than $20K"

prop.table(table(gss2k$income2, gss2k$region), margin = 2)

prop.table(table(gss2k$income2, gss2k$region), margin = 1)







med_age_sex <- tapply(gss2k$age, gss2k$sex, median, na.rm = TRUE)

tapply(gss2k$age, gss2k$income2, median, na.rm = TRUE)

which.min(tapply(gss2k$age, gss2k$income2, median, na.rm = TRUE))


which.max(prop.table(table(gss2k$income2, gss2k$region), margin = 1))




