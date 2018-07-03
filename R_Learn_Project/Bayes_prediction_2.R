Train <- read.csv("cancer_train.csv")
Test <- read.csv("cancer_test.csv")

library(e1071)
library(caret)
library(caTools)
library(ROCR)

model <- naiveBayes(Class~., data = Train)
pred <- predict(model, newdata = Test, type = "raw")
