# The following dataset consist of data on football games for 11
# european countries.  It covers time from 2011-2016. The variable
# result has two arguments: HDW- Home team didnt win, HW- Homw
# team won. Most of the data describes the players strength
# averaged for the last 7 games (prior to the given game) averaged
# by the team (those are only for players who participated in the
# game).  More information can be found here-
# http://sofifa.com/player/192883.  You can find a huge amount of
# analytics done on football data
# here-https://www.kaggle.com/hugomathien/soccer variables
# 'home_team_points', 'away_team_points' show the amount of the
# points the teams have earned before the game (3 for win, 1 for
# draw, 0 for lose) Variable stage shows the Round number during
# the season. The poitive case for the model is HW (home team
# won).

## libraries that may be needed
library(lattice)
library(ggplot2)
library(e1071)
library(caret)
library(ROCR)
library(caTools)
library(class)
library(randomForest)
library(AUCRF)

# Dont forget to set the seed everytime you run randomForest.
# Divide data into training and testing set,80% goes to train.

load("Soccer.rda")
set.seed(2016)
index <- createDataPartition(soccer$result1, p=0.8, list=F)
train <- soccer[index,]
test <- soccer[-index,]

# Question 1. Do some descriptive analytics (charts, tests, etc)
# to find interesting patterns in the data (10 points)

summary(soccer$result1)
## HDW - 10589  and HW - 8800
## Most of the teams tend to win at home

pairs(home_balance ~ home_strength, data = soccer) # balance/strength correlation for home
pairs(away_balance ~ away_strength, data = soccer) # balance/strength correlation for away

qplot(home_potential, data = soccer, binwidth = 0.8) # the concentration is between 70 and 80, pretty high numbers
qplot(home_heading_accuracy, data = soccer, binwidth = 0.8) # the concentration is between 55 and 65
qplot(away_heading_accuracy, data = soccer, binwidth = 0.8) # the concentration is between 55 and 70, it is clearly better
# so home heading accuracy is worse than away heading accuracy.

## data has too many variables, so we divide
cor(soccer[, 1:35])
cor(soccer[, 36:71])

## home potential is highly correlated with home short passing, home ball control, home reactions
## away vision is is highly correlated with awal short passing, away long passing, ball control

# Question 2. Build a
# random forest model with the package randomForest. Your goal is
# to predict the game result (variable 'result1') (15 points)

set.seed(2016)
model_rf1 <- randomForest(result1 ~ ., data = train, ntree = 1000, 
                          nodesize = 40, mtry = 5, maxnodes = 20, importance = T, 
                          do.trace = T, localImp = T, replace = F)

pred1 <- predict(model_rf1, newdata = test)
pred1_prob <- predict(model_rf1, newdata = test, type = "prob") # predicted probabilities
pred1_prob
confusionMatrix(pred1, test$result1, positive = "HW") # accuracy = 0.6314
varImpPlot(model_rf1, sort = T, type = 1, n.var = 40) 

# 2.1 Develop randomForest model by tunning several parameters.
# Look for package help for more info.  Explain the meaning of
# each parameter.

## ntree: Number of trees to grow.
## nodesize: This is the minimum node size, it implicitly sets the depth of your trees.
## mtry: Number of variables randomly sampled as candidates at each split.
## replace: being false means that we want model without replacement of cases
## importance: the importance of predictors is being computed
## do.trace: is a stoping criterea in case the error remains the same and the tree grows

set.seed(2016)
mtry_tune <- tuneRF(soccer[, -72], soccer[, 72], stepFactor = 1.5, 
                 trace = TRUE, ntreeTry = 500, improve = 0.1, plot = TRUE, doBest = TRUE)

set.seed(2016)
model_rf2 <- randomForest(result1 ~ ., data = train, ntree = 500, 
                          nodesize = 80, mtry = 8, maxnodes = 40, importance = T, 
                          do.trace = T, localImp = T, replace = F) # this configuration seems to be quite good


# 2.2 Report on accuracy of your final chosen model (OOB
# estimate). Comment on it

pred2 <- predict(model_rf2, newdata = test)
confusionMatrix(pred2, test$result1, positive = "HW")
## accuracy is 0.6428


print(model_rf2)
##OOB estimate is 35.36%

## this accuracy is not the highest possible, but it is the best I could get by simply tuning the parameters
## nodesize of 80 seems a bit too high but, it works well, maxnodes = 40 seems to give the best cut for the trees,
## and the mtry that I got trough tuneRF function is the one that gives the minimal error among those which I tried

# 2.3 Report AUC on testing set.

pred2_prob <- predict(model_rf2, newdata = test, type = "prob")
pred_final <- prediction(pred2_prob[, 2], test$result1, label.ordering = c("HDW", "HW"))
perf_final <- performance(pred_final, "tpr", "fpr")
plot(perf_final)
performance(pred_final, "auc")@y.values #auc= 0.6940419

# 2.4 What are the most important variables?

varImpPlot(model_rf2, sort = T, type = 1, n.var = 40)

## let's say the most important ones are the following
## away_ball_control, away_vision, home_short_passing, home_ball_control, away_potential

# Question 3. Use caret to train randomforest model. Think about
# the hyperparameters you can use for model tuning.  Do grid
# search.Hint: play with expand.grid parameter.  Report the best
# model. (15 points).

set.seed(2016)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(train))))
modellist <- list()

set.seed(2016)
fit <- train(result1 ~ ., data = train, method = "rf", metric='Accuracy', 
             tuneGrid = tunegrid, trControl = control)

print(rf_gridsearch)
plot(rf_gridsearch)

## This took more than 2 hours on my i7 pc and I still feel like Hachiko :) it didn't finish
## but I guess this is correct way

# Question 4. there is a package 'AUCRF' that uses randomForest
# but reports AUC on the OOB sets.  Use it to build randomforest
# model (10 points)

aucrf_train <- train # to make changes to the result1 (make 0/1)
aucrf_train$result1 <- factor(aucrf_train$result1, levels=c('HDW', 'HW'), labels = c(0,1))

set.seed(2016)
model_aucrf <- AUCRF(result1 ~ ., data = aucrf_train, ranking="MDA")
plot(model_aucrf)
summary(model_aucrf) # AUC of selected variables: OOB-AUCopt= 0.7046464 


# Bonus question (15 points). 
# The variables in the dataset are the
# same measures of home and away teams. For example
# 'away_short_passing' and 'home_short_passing' are showing how
# good are both teams in short passing. Now think what kind of
# transformations can you do with the data to decrease the number
# of variables and get better model. Report your way of thinking
# and the final model.

## So if there are symmetric variables
## I think that taking the averages for each column and making 2 variables 1 is a nice opportunity
## I think of a trick, where I can remove the first part of the string and get the names of the variables generalized
## I expect to get a huge decrease in the accuracy, as this way is making data less informative

soccer_new <- soccer[, c(1:3,72)]
soccer_columns <- colnames(soccer)

# starting column indeces for home/away variables
i_home = 4 
i_away = 38

repeat { 
  if (i_home <= 37) 
  { 
    home_cols <- soccer_columns[i_home]
    away_cols <- soccer_columns[i_away]
    
    columns_final <- substr(home_cols, 6, nchar(home_cols)) # generalizing the variable name
    soccer_new[[columns_final]] <- (soccer[[home_cols]] + soccer[[away_cols]])/2
    
    i_home = i_home + 1 # no increment operator :(
    i_away = i_away + 1
  }
  else{break}
}

set.seed(2016)
index <- createDataPartition(soccer_new$result1, p = 0.8, list = FALSE)
train_final <- soccer_new[index,]
test_final <- soccer_new[-index,]

set.seed(2016)
model_rf_final <- randomForest(result1 ~ ., data = train_final, ntree = 500, 
                          nodesize = 80, mtry = 8, maxnodes = 40, importance = T, 
                          do.trace = T, localImp = T, replace = F)

pred_final <- predict(model_rf_final, newdata = test_final)
confusionMatrix(pred_final, test_final$result1, positive = "HW")
## Accuracy = 0.5798
## It's low, as I was expecting
