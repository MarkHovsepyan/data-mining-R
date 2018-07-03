#----Homework 1----
# For your homework you will need two datasets: Movies and Employee Attrition.
# Please load them into R so that you can start working on them.
# Make sure you provide enough description for your findings whenever question asks
# for that. Feel free to express your thoughts, use your own words.
# Write your code right below the questions.
# Submit your R script on the moodle.
# The code should be written in a way that I will be able to open and run it
# on my computer without getting any errors.
# For the upcoming questions, use movies database.

load("Movies.rda")

#libraries to use
library(ggplot2)
library(plyr)

# 1.What are the earliest and latest years for which we have a movie in the dataset? (1 point)

range(movies$Year) 
#min: 1920, max: 2016

# 2.Create distribution of movies by years. What does this tell you? (1 point)

qplot( movies$Year, bins = (2016 - 1920) )

year_freq_table <- table(movies$Year)
View(year_freq_table)
which.max(year_freq_table) # 2006
which.min(year_freq_table) # 1920
mean( movies$Year )   # 2002.988
median( movies$Year ) # 2005

#We can conclude that the least number of movies was created in 1920 (1 movie)
#and that the most number of movies was created in 2006 (189 movies)

# 3.Look at the average imdb rating for movies by year. Do you see any trend? (1 point)

imdb_rating_avg <- aggregate(movies$imdbRating, by = list(movies$Year), FUN = "mean", na.rm = TRUE)
colnames(imdb_rating_avg) <- c( 'Year', 'Rating' )
imdb_rating_avg$year[which.max( imdb_rating_avg$rating )]
imdb_rating_avg$year[which.min( imdb_rating_avg$rating )]

qplot(Year, Rating, data = imdb_rating_avg)
plot(imdb_rating_avg$Year, imdb_rating_avg$Rating, type = 'o' )

#after 1978 the average rating tends to get around 6.5 for all the following years
# maximum avarage rating was in 1996 and minimum average rating was in 1920

# 4.Create a boxplot for imdbRating and year, what do you see? (2 points)

boxplot(movies$imdbRating~movies$Year)

#the highest mean was nearly in 1970
#after 2005 averages get closer to each other

# 5.Create a new dataframe which is a subset of the old one.
#The dataframe must contain only the years during which at least 10 movies were
#created. Call the new dataframe movies2. (4 points)

frequency <- data.frame(table(unlist(movies$Year)))
colnames(frequency) <- c('Year', 'Frequency')
movie_years <- subset(frequency, subset = frequency$Frequency >= 10)
movies2 <- subset(movies, subset = movies$Year %in% movie_years$Year )
View(movies2)
str(movies2)

# For the upcoming questions use the movies2 dataframe.
# 6.Create a histogram of imdbRating variable. What does it tell you? (1 point)

hist(movies2$imdbRating)
#ratings that occur most frequently are between 6 and 8 (nearly 7)

# 7.Create histogram for a variable tomatoUserRating. What does it tell you?
# Compare with histogram of imdbRating - any insights? (2 points)

hist(movies2$imdbRating)
hist(movies2$tomatoUserRating*2, add = T, col = rgb( 0.5, 0.2, 0.6, 0.7 )) 
#multiply by 2 as it is scaled in 5 point system 
#in tomatoUserRatings the most frequent ratings are between 2.5 and 3.5 (nearly 3)
#both graphs are quite similar

# 8.Metascore is a score given to the movie by the critics, while imdbRating
# is a rating given by the users.
# What is the meaning of correlation coefficient between these two variables?
# (Hint: cor function will give an error if you dont handle missing values.
# Look at the help for the function to fix that issue). (2 points)

cor(movies2$imdbRating, movies2$Metascore, use = "complete.obs")
#correlation is quite high (more than 0.7)

# 9.Find the movie for which critics and users disagree the most. (Hint:
# You may need to pay attention to the scales of the ratings) (2 points)

X <- na.omit(movies2[,c('Metascore', 'imdbRating', 'Title')])
disagreement <- data.frame(abs(X$Metascore - 10 * X$imdbRating))
colnames(disagreement) <- c('Difference')
X$Title[which.max(disagreement$Difference)]

# 10. Create correlation matrix between all rating variables.
# (Hint: Read the descriptions of all the variables to see which of them are ratings.) (1 point)
pairs(~Metascore+imdbRating+tomatoRating+tomatoUserRating+tomatoMeter+tomatoUserMeter, data = movies2) #plot
cor(movies2[,c("Metascore","imdbRating", "tomatoRating", "tomatoUserRating", "tomatoMeter", "tomatoUserMeter")], use = "complete.obs") #table

# 11.Do your own research and explain the meanings of the signs of 
# correlation  coefficients.(2 points)

# when sign is + and near 1 it means that correlation is significant
# when cor() is 0 it means that there is no correlation
# when sign is - and near -1 it means that the difference is high and one grows in the opposite direction of the other

# 12.Create a new dataframe which will show mean 
# gross income generated for each year. Create a plot summarizing that data.
# What trend do you see. Explain. (3 points)

movies_gross <- aggregate(movies2$gross, by = list(movies2$Year), FUN = "mean")
colnames(movies_gross) <- c("Year", "Gross")
movies_gross$Year[which.max(movies_gross$Gross)] # max is 2016
movies_gross$Year[which.min(movies_gross$Gross)] # min is 1999
qplot(Year, Gross, data = movies_gross)

# until 1999 there is a decrease in gross and after 1999
# there is an increase in gross
# reached max in 2016

# 13.What do you think, which variable is mostly correlated with the gross income.
# Test your assumption using cor function. Did you guess correct? Elaborate. (2 points)

cor(movies2$gross, movies2$imdbRating, use = "complete.obs")
cor(movies2$gross, movies2$budget, use = "complete.obs")
cor(movies2$gross, movies2$tomatoUserRating, use = "complete.obs")
cor(movies2$gross, movies2$imdbVotes, use = "complete.obs") #this is the highest correlation

# 14.Which movie director has the highest average imdbRating? Is his average gross
# also the highest. Give your thoughts on this topic.(Hint: you might find useful
# the phenomenon of time value of money). (3 points)

movies_direct_rating <- aggregate(movies2$imdbRating, by = list(movies2$director_name), FUN = "mean")
movies_direct_gross <- aggregate(movies2$gross, by = list(movies2$director_name), FUN = "mean")
colnames(movies_direct_rating) <- c( 'Name', 'Rating' )
colnames(movies_direct_gross) <- c( 'Name', 'Gross' )
max(movies_direct_rating$Rating, na.rm = TRUE)
movies_direct_rating[which.max(movies_direct_rating$Rating),]
max(movies_direct_gross$Gross, na.rm = TRUE)
movies_direct_gross[which.max(movies_direct_gross$Gross),]

# highest average rating has Tony Kaye (8.6)
# however, highest gross averag has Lee Unkrich (414984497)

# 15.Do your own analysis. Find something interesting (aggregate the data, 
# create plots, etc) (5 points)

nums <- sapply( movies2, is.numeric )
cor( movies2[ ,nums ], use='complete.obs' )

#variables that I found out to be highly correlated:

qplot(movies2$actor_1_facebook_likes, movies2$cast_total_facebook_likes)  # highly corelated
cor(movies2$actor_1_facebook_likes, movies2$cast_total_facebook_likes)    # 0.9460783 (near 1)
t.test(movies2$actor_1_facebook_likes, movies2$cast_total_facebook_likes) # p-value < 2.2e-16

qplot(movies2$tomatoUserMeter, movies2$tomatoUserRating)                  # highly corelated
t.test(movies2$tomatoUserMeter, movies2$tomatoUserRating)                 # p-value < 2.2e-16

qplot(movies2$tomatoRating, movies2$Metascore)                            # highly corelated
t.test(movies2$tomatoRating, movies2$Metascore)                           # p-value < 2.2e-16

#I didn't manage to find other highly correlated variables

# For the upcoming questions, use Employee attriotion database.
# Factors are not set to this data, so R recognises them as integers.
# For some problems you might need setting factors in order for R to operate correctly.
# Make sure your variable types are set correctly before writing the code.

load("Employee Attrition.rda")

# 16.Which position of employee (JobRole) is yielding maximum salary (HourlyRate) 
# on average? Representative of which position (JobRole) has the highest
# hourly rate according to dataset? (2 points)

attrition1 <- aggregate(attrition$HourlyRate, by = list(attrition$JobRole), FUN = "mean", na.rm = TRUE)
attrition1[which.max(attrition1$x),]

#Healthcare Representative is the position with the highest hourly income

# 17.What is the correlation between the age of the employee and the hourly rate. Can you
# say that the older is the employee, the higher he is earning? Explain. (1 point)

cor(attrition$Age, attrition$HourlyRate)
qplot(attrition$HourlyRate, attrition$Age)

# 18.What is the age of the youngest and the oldest employees in the data? (1 point)

range(attrition$Age)
# youngest: 18, oldest: 60


# 19.Create a histogram of the employees by monthly income. What does it tell you? (1 points)

hist(attrition$MonthlyIncome)

# most of the employees' salary is in range of 2000 to 7000
# highest are between 18000 and 20000

# 20.Create a boxplot with Eduation and Percent Salary Hike. What do you see? (2 points)
# Explain your findings.

boxplot(attrition$Education~attrition$PercentSalaryHike)

#PercentSalaryHike is not dependent on Education level

# ----Hypothesis testing----
# 21. Is there a difference between average Monthly Income of employees that churn
# (leave the company) and those who stay within the company? (2 points)

t.test(attrition$MonthlyIncome~attrition$Attrition)
# p-value = 4.434e-13 it means they are correlated as p-value -> 0


# 22.Is the decision of attriting from the company independent from the gender of
# the employee? (2 points)

chisq.test(attrition$Attrition, attrition$Gender)
# there is no signficant relationship between them as p-value = 0.2906, but still there is small connection

# 23. Choose any two variables that you want and conduct a test either for independence
# or for differences in means. (2 points)

cor(attrition$DistanceFromHome, attrition$DailyRate)        # -0.004985337 => no correlation which means they are independent
chisq.test(attrition$DistanceFromHome, attrition$DailyRate) # p-value = 0.9608 => no correlation which means they are independent
plot(attrition$DistanceFromHome, attrition$DailyRate)

# 24. Do your own analysis for attrition data. Find something
# interesting (aggregate the data, create plots, etc) (5 points)

variables <- sapply(attrition, is.numeric)
cor(attrition[,variables], use = "complete.obs") # all correlations between variables

boxplot(attrition$MonthlyIncome ~ attrition$JobLevel)             # as expected, they are highly correlated
t.test(attrition$YearsInCurrentRole, attrition$YearsWithCurrManager)  # most of the workers that stay at the ssame role prefer to stay with the same manager

boxplot(attrition$MonthlyIncome ~ attrition$Education)             # highly correlated

t.test(attrition$Age, attrition$TrainingTimesLastYear)  # there is some correlation, which means age is affecting the training amount the employee gets
