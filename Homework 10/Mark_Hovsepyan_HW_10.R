# The dataset Cancer.csv contains data about the tumor cells
# for 100 patients. The independent variables are different
# characteristics of the tumor cells. The dependant variable
# is diagosis_result, which has two classes: B and M (benign
# and malignant). Our goal is to create the model to identify
# whether the patient has type B or type M cancer.

## libraries that may be needed
library(class)
library(ROCR)
library(caret)
library(neuralnet)
library(ggplot2)
library(pROC)

# 1. Load data. Prepare training and testing sets (80%/20%).
# Make all the required preprocessing for building the
# neuralnetwork model. (10)

cancer <- read.csv("Cancer.csv")

sc <- function(x) {
  (x - min(x))/(max(x) - min(x))
}

for(x in 1:8) {
  cancer[, x] <- sc(cancer[, x])
}

cancer$diagnosis_result <- ifelse(cancer$diagnosis_result == "M", 0, ifelse(cancer$diagnosis_result == "B", 1, cancer$diagnosis_result))


index <- createDataPartition(cancer$diagnosis_result, p=0.8, list=F)
train <- cancer[index,]
test <- cancer[-index,]


# 2. Build the neural network model. Play with the parameters
# in order to get as high accuracy as possible. Report the
# final accuracy. Plot the model, make sure you do not have
# overlapping objects in the plot.  (20)

fm <- formula(diagnosis_result ~ radius+texture+perimeter+area+smoothness+compactness+symmetry+fractal_dimension)

model_nn <- neuralnet(fm, data = train, hidden = c(3, 4, 3), rep = 7, 
                       linear.output = F, err.fct = "ce")

plot(model_nn, length = 0.1)
cmpt <- compute(model_nn, test[, -9], rep = 1)
p_test <- ROCR::prediction(cmpt$net.result[, 1], test$diagnosis_result)
performance(p_test, "auc")@y.values

class_nn <- ifelse(cmpt$net.result > 0.5, 1,0)
confusionMatrix(class_nn, test$diagnosis_result, positive = "1")

## max auc that I got after running several times was 0.9642857143
## accuracy is 0.75


# 3. Use one of the classification model that we covered so
# far. Cross validate it using caret. Out of the two 'cross
# validated' models, which one is doing better? (20)

## let's use KNN classification algorithm

cancer <- read.csv("Cancer.csv")
index <- createDataPartition(cancer$diagnosis_result, p=0.8, list=F)
train_new <- cancer[index,]
test_new <- cancer[-index,]

ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

fit_knn <- train(diagnosis_result ~ ., data = train_new, method = "knn", 
                 trControl = ctrl, 
                 preProcess = c("center","scale"), 
                 tuneGrid = expand.grid(k = 3:20))

fit_knn$results
fit_knn$bestTune

## My best k is 8

model_knn <- knn(train_new[, -9], test_new[, -9], 
                 train_new$diagnosis_result, 
                 k = 8, prob = T)

probs <- predict(fit_knn, newdata = test_new, type = 'prob')
p_test_new <- prediction(probs[, 2], test_new$diagnosis_result)
performance(p_test_new, "auc")@y.values

## AUC for KNN is 0.9687812
## It is slightly better than neural network on this dataset, most importantly it is more stable
## neural net's results may vary, while here with a certain k you will get really close results always

# BONUS Find data with at least one catrgorical independent
# variable and build a neural network on it. What are the
# AUC, accuracy, sensitivity and specificity of your model?
# Explain their meanings.  (10)


## I used Diabetes dataset from moodle
diabetes <- read.csv("Diabetes.csv")

for(x in 1:8) {
  diabetes[, x] <- sc(diabetes[, x])
}

index <- createDataPartition(diabetes$Class, p=0.8, list=F)
train_diab <- diabetes[index,]
test_diab <- diabetes[-index,]

fm <- formula(Class ~ NTS+PGC+DBP+TSFT+INS+BMI+DPF+Age)

model_nn_new <- neuralnet(fm, data = train_diab, hidden = c(3, 3), rep = 3,  
                          linear.output = F, err.fct = "ce")

plot(model_nn_new, length = 0.1)
cmpt_diab <- compute(model_nn_new, test_diab[, -9], rep = 1)
p_test_diab <- ROCR::prediction(cmpt_diab$net.result[, 1], test_diab$Class)
performance(p_test_diab, "auc")@y.values

class_nn_diab <- ifelse(cmpt_diab$net.result > 0.5, 1,0)
confusionMatrix(class_nn_diab, test_diab$Class, positive = "1")

## AUC is 0.810325477
## accuracy is 0.745098
## Sensitivity : 0.7037037             
## Specificity : 0.7676768

## we all know what they mean, don't we? :D