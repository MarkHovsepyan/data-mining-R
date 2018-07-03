# The dataset credit contains information about the bank customers.
# It is used to identify whether a customer who applied for the
# loan will default or not.

## libraries that may be needed
library(e1071)
library(caret)
library(caTools)
library(ROCR)
library(klaR)

#1. Load credit data into R. Make sure the categorical
# variables are factored. Create testing and training 
# datasets, so that 80% of data goes to train and the rest
# goes to test. Make sure the proportions of the dependent 
# variable are fixed. Set the seed to 2016. (7)

credit <- read.csv("Credit.csv")
credit$ed <- as.factor(credit$ed)
credit$default <- as.factor(credit$default)
credit$default <- factor(credit$default, 
                         levels = c(0,1),labels = c("Not Defaulted","Defaulted"))

set.seed(2016)
index <- createDataPartition(credit$default, p = .8, list = FALSE)                
Train <- credit[index,]
Test <- credit[-index,]


#2. Create a naive bayes model on the dataset. Set 
# Laplace equal to 1. What is the accuracy of the model? (7)

nb_model <- naiveBayes(default~., data = Train, laplace = 1)

pred_test <- predict(nb_model, newdata = Test)

pred_test_prob <- predict(nb_model, newdata = Test, type = "raw")

predictions <- prediction(pred_test_prob[, 1], Test$default, label.ordering = c("Defaulted", "Not Defaulted"))

confusionMatrix(pred_test, Test$default, positive = "Defaulted") # cnofusion matrix
## Accuracy : 0.741
## Sensitivity : 0.27778
## Specificity : 0.90291

#3. Plot the ROC curve, make sure you have the colors of the
# thresholds on the curve. Give explanation to the coloring of
# the curve: what does it show?? What is the AUC? (7)

perf <- performance(predictions, "tpr", "fpr")
plot(perf, colorize = TRUE)
performance(predictions, "auc")@y.values

## The area under the curve is 0.7208738

#4. Given that someone defaulted, what is the probability that
# he/she has postgarduate degree? (7)

nb_model$tables # conditional probabilities tables

## Given that someone defaulted,
## the probability of having post_graduate degree is 0.01315789
## Note: this is for Train dataset, not for the whole credit data

#5.Take any of the classification methods that we studied so
# far and build a model using it. Compare that model with the
# Naive Bayes model. Which one does better? Comment. (7)

## LDA model
LDA_model <- lda(default~., data = Train)
LDA_pred <- predict(LDA_model, newdata = Test)

confusionMatrix(LDA_pred$class, Test$default, positive = "Defaulted")

## LDA model did a bit better job with the following results
## Accuracy = 0.777
## Sensitivity : 0.27778
## Specificity : 0.90291  

## Accuracy is better 0.777 > 0.741

## GLM model
GLM_model <- glm(default~ ., data = Train, family = "binomial")
GLM_pred <- predict(GLM_model, newdata = Test, type = "response")

labels <- ifelse(GLM_pred > 0.5, "Defaulted", "Not Defaulted")
confusionMatrix(labels, Test$default, positive = "Defaulted")

## GLM model has even better results
## Accuracy : 0.7986 > 0.741
## Sensitivity : 0.5556 > 0.27778
## Specificity : 0.8835 < 0.90291 (except this one)

#6. Load the scoring datset into R. Our goal will be to give
# credit scores (defualting probabilities) to the potential
# customers. Predict the scores with the Naive Bayes model. (8)

scoring <- read.csv("Scoring.csv")
scoring_predict <- predict(nb_model, newdata = scoring, type = "raw")
scoring$default <- scoring_predict[, 2]

#7. Identify the top 25% of customers that are least risky.
# Describe them with the variables you have in the scoring dataset.(7) 

leastRisky_25 <- head(scoring[order(scoring$default, decreasing= FALSE),], n = nrow(scoring)/4)

print(average_age_top <- mean(leastRisky_25$age))
print(average_income_top <- mean(leastRisky_25$income))
print(average_debt_card_top <- mean(leastRisky_25$creddebt))
print(debt_income__average_top <- mean(leastRisky_25$debtinc))

table(leastRisky_25$ed)

## Aveerage age = 40.64865~41
## Average income = 54.40541
## Average credit card debt ~ 1097
## Average ratio of debt to income = 7.727027

## data from education degree table
## 1 has high school degree
## 36 people do not have even high school degree

# Bonus point
#8. Compare top 25% of risky customers (Quartile 4) with bottom 25% of risky customers (Quartile 1). 
# What are the main differences you see? Generate 1-2 tables and graphs for the analysis. (10 points)

bottomRisky_25 <- tail(scoring[order(scoring$default, decreasing = FALSE),], n = nrow(scoring)/4)
print(averageAge_bottom <- mean(bottomRisky_25$age))
# Average age for bottom 25% = 35.84211 ~ 36 years
print(averageIncome_bottom <- mean(bottomRisky_25$income))
# Average income for bottom 25% ~ 79473
print(average_debt_card_bottom <- mean(bottomRisky_25$creddebt))
# Average credit card debt ~ 3472
print(debt_income__average_bottom <- mean(bottomRisky_25$debtinc))
# Average ratio of debt to income = 12.96316

table(bottomRisky_25$ed)
hist(bottomRisky_25$ed) # education level histogram
## Amazingly high number of people with college degree compared to other education levels
## 2nd most are post-graduate degree people

cor(bottomRisky_25$income, bottomRisky_25$ed)
pairs(bottomRisky_25) # some correlations of variables

## Average age comparison : 36 < 41
## Average incomecomparison : 79473 > 54405
## Average credit card debt comparison : 3472 > 1097
## Average ratio of debt to income comparison : 12.96316 > 7.727027

## here the level of education is much higher in average than in top 25%