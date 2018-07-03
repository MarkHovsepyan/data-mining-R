#K_Nearest Neighbors 10/17/2016
#tell me, who are your friends, I will tell you who you are
knn_example <- read.csv("KNN small example.csv")
View(knn_example)
knn_example$Default <- factor(knn_example$Default, levels = c(0,1), labels = c("no", "yes"))

library(ggplot2)

qplot(Age, Loan, data = knn_example, colour=Default, size= I(5))
#let's say k=3, means we look at 3 nearest neighbors
e <- c(30, 130000, NA)
#to add row use  rbin()
knn_example <- rbind(knn_example, e)
qplot(Age, Loan, data = knn_example, colour=Default, size= I(5))
#new case appears blackas it does not have  a membership


# diabetes dataset
diabetes <- read.csv("Diabetes.csv")

diab_scale <- as.data.frame(scale(diabetes[,1:8]))
diab_scale$Class <- factor(diabetes$Class, levels = c(0,1), labels = c("Positive", "Negative"))

library(caTools)
library(caret)

set.seed(1313)
Index <- createDataPartition(diab_scale$Class, p = 0.8, list = FALSE)
Train <- diab_scale[Index,]
Test <- diab_scale[-Index,]

library(class)

model <- knn(Train[, 1:8], Test[, 1:8], Train$Class, k = 3) # model for k number
model

confusionMatrix(model, Test$Class, positive = "Positive") # for finding accuracy


# cross validation
install.packages("pROC")
library(pROC)

diabetes$Class <- factor(diabetes$Class, levels = c(0, 1), labels = c("Negative", "Positive")) # just factor

ctrl <- trainControl(method = "repeatedcv", number = 10, 
                     repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)

set.seed(2015)
knnFit <- train(Class~., data = Train, trControl = ctrl, 
                method = "knn", preProcess = c("center", "scale"),
                tuneGrid = expand.grid(k = 5:10))

plot(knnFit)
knnFit$results
knnFit$bestTune


# Create the ROC curve
library(ROCR)
P_Test <- prediction(knn_probs[,2], Test$Class) 
perf <- performance(P_Test,"tpr","fpr")
plot(perf)
##Find Area under the curve (AUC)
performance(P_Test, "auc")@y.values



