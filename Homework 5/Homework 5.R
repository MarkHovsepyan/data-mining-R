# Homework 5- KNN
# The dataset Cancer.csv contains data about the tumor cells for 
# 100 patients. The independent variables are different
# characteristics of the tumor cells. The dependant variable
# is diagosis_result, which has two classes: B and M (benign and
# malignant). Our goal is to create the model to identify whether
# the patient has type B or type M cancer. 
# Attention!!!- while solving the problems, make sure to set the 
# seed to 2016 every time when you are using a function that
# does any random operation. 

#1. Load the data to R and scale it. (5.5)

cancer_data <- read.csv("Cancer.csv")
cancer_scale <- as.data.frame(scale(cancer_data[, 1:8]))
cancer <- cancer_scale
cancer$diagnosis_result <- cancer_data$diagnosis_result

#2. Set seed to 2016. Create testing and training sets. 
# 80% of data should go to train, the rest to test.
# Make sure the proportions of the categorical variable are 
# not changed (some small variation is ok). (5.5)

library(caTools)
library(caret)
library(class)
library(pROC)
library(ROCR)
## some libraries that might be needed

set.seed(2016)
Index <- createDataPartition(cancer$diagnosis_result, p = 0.8, list = FALSE)
Train <- cancer[Index,]
Test <- cancer[-Index,]

#3. Using library caret, identify the optimal number of K's.
# Do repeated k-fold cross validation.
# Use the accuracy for defining which number is the best.
# Also, do not forget to set seed to 2016. (5.5)

model <- knn(Train[, -9], Test[, -9], Train$diagnosis_result, k = 29) # model for k = 29
model
confusionMatrix(model, Test$diagnosis_result, positive = "M") # for finding accuracy
## 29 seems to be the best number for k (Accuracy = 0.9474)

set.seed(2016)
ctrl1 <- trainControl(method="repeatedcv",
                     number = 10, repeats = 5) 

set.seed(2016)
knnFit1 <- train(diagnosis_result ~ ., data = Train, method = "knn",
                trControl = ctrl1, preProcess = c("center","scale"), 
                tuneGrid = expand.grid(k = 3:32))

plot(knnFit1) # creating a plot
knnFit1$results # look at results
knnFit1$bestTune # 29 again

#4. Now do the same analysis, but use AUC (area under the curve)
# for identifying which number of K's is the best. 
# Is that number the same as in the prvious case?
# Again, set the seed to 2016. (5.5)

set.seed(2016)
ctrl2 <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary) 

set.seed(2016)
knnFit2 <- train(diagnosis_result ~ ., data = Train, method = "knn",
                trControl = ctrl2, 
                preProcess = c("center","scale"), 
                tuneGrid = expand.grid(k = 3:32))

plot(knnFit2)
knnFit2$results
knnFit2$bestTune

#5. Based on the results in the previous two problems, choose
# the most optimal number of K's. Run KNN classification
# using the library class. (5.5)

set.seed(2016)
KNN_result <- knn(Train[,-9], Test[,-9], cl = Train$diagnosis_result, prob = TRUE, k = 29) # I go with 29
table(KNN_result, Test$diagnosis_result) # confusion matrix

#6. What is the accuracy of that model? Use the confusionMatrix
# to get the number. The positive class is M. (5.5)

confusionMatrix(KNN_result, Test$diagnosis_result, positive="M") # accuracy + confusion matrix
## accuracy is 0.9474

#7. Look at the sensitivity and specificty of the models. 
# Explain their meanings within the context of the problem.
# How will you deal with the classification threshold to balance
# the risk of having false negative and false positive results?
# Explain. (5.5)

## specificity: 0.85
## sensitivity: 1

## so, here sensitivity is 1, which means there are no false positive results
## all that show "M" are indeed "M"
## specificity is 0.85, as there is one false negative value
## it shows "B", but in reality it is "M"

attr(KNN_result, "prob") # view probabilities
## if we pick the threshold >= 0.7, then the risk will be minimal, 
## as all the low probability ones will be excluded and those that are near each other wil be at high probability rate

#8. Now solve the same classification problem using logistic
# regression. What is the accuracy of your model? (5.5)

model_new <- glm(diagnosis_result ~., data = Train, family = "binomial")
summary(model_new)
pred <- predict(model_new, newdata = Test, type = "response")
summary(pred)
hist(pred) # histogram to visualize it better
table(Predicted = pred > 0.5, Actual = Test$diagnosis_result) # confusion matrix
p_test_new <- prediction(pred, Test$diagnosis_result)
perf_new <- performance(p_test_new, "tpr", "fpr")
plot(perf_new)

#9. Which classsification model works better? 
# Compare several accuracy measures from confusion matrix and 
# give your thoughts. (6)

## As I can say from this experiments, KNN gives much better results
## I got much higher accuracy with KNN classification than in the glm model
## One of the reasons may be that KNN uses a substantial number of tryouts,
## whereas in glm model I choose myself which variables are essential for the model and which ones are not
## Moreover, Sensitivity and Specificity are both better when I use KNN
