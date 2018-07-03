# The data frame voice contains information about
# the voices of females and males. It summarizes
# several measurements for voice and the label,
# indicating the gender of the 'owner' of the
# voice.  
# Do not forget to set the seed to 2016 for
# all the functions that need random activity.  

## libraries that may be needed
library(e1071)
library(caret)
library(caTools)
library(ROCR)
library(klaR)
library(ggplot2)
library(class)
library(MASS)

# 1. Load voice data. Create testing and training
# sets. 80% goes to train, the rest to test. (7)

voice <- read.csv("voice.csv")

set.seed(2016)
index <- createDataPartition(voice$label, p = 0.8, list = FALSE)
train_voice <- voice[index,]
test_voice <- voice[-index,]

# 2. Create svm model, try different types of
# kernels.  Which one is giving the highest
# accuracy? Hint: Look at the help of the svm
# function to see what types of kernels are
# avilable.(7)

## Kernel types: linear, polynomial, sigmoid, radial basis

## linear kernel
set.seed(2016)
model_linear <- svm(label ~ ., data = train_voice, kernel = "linear", probability = TRUE)
pred_linear <- predict(model_linear, newdata = test_voice, probability = TRUE)
confusionMatrix(pred_linear, test_voice$label)
## Accuracy = 0.9715

## polynomial kernel
set.seed(2016)
model_polynomial <- svm(label ~ ., data = train_voice, kernel = "polynomial", probability = TRUE)
pred_polinomial <- predict(model_polynomial, newdata = test_voice, probability = TRUE)
confusionMatrix(pred_polinomial, test_voice$label)
## Accuracy = 0.9636

## sigmoid kernel
set.seed(2016)
model_sigmoid <- svm(label ~ ., data = train_voice, kernel = "sigmoid", probability = TRUE)
pred_sigmoid <- predict(model_sigmoid, newdata = test_voice, probability = TRUE)
confusionMatrix(pred_sigmoid, test_voice$label)
## Accuracy = 0.7911

## radial basis kernel
set.seed(2016)
model_radial <- svm(label ~ ., data = train_voice, kernel = "radial", probability = TRUE)
pred_radial <- predict(model_radial, newdata = test_voice, probability = TRUE)
confusionMatrix(pred_radial, test_voice$label)
## Accuracy = 0.981

## We get the highest accuracy with radial basis kernel, that is 0.981


# 3. Take the type of the kernel that gives the
# highest accuracy and proceed with it. Plot the
# ROC taking female as the class of interest. (7)

radial_predict <- predict(model_radial, newdata = test_voice, 
                                  probability = TRUE, type = "raw") 
radial_predict <- attr(radial_predict, "probabilities")

p_test_radial_f <- prediction(radial_predict[,2], test_voice$label, label.ordering = c("male", "female"))
perf_radial_f <- performance(p_test_radial_f, 'tpr', 'fpr')
plot(perf_radial_f)

# 4. Using library caret, do cross validation and
# find the optimal value of cost. What is the value
# of accuracy while using that value of cost? Use the
# seed of 2016. (8)

set.seed(2016)
ctrl <- trainControl(method = "repeatedcv", number = 12, repeats = 4)

set.seed(2016)
fit_svm <- train(label ~ ., data = train_voice, trControl = ctrl, 
                 method = "svmRadialCost", tuneLength = 10)
fit_svm$results
fit_svm$bestTune

## The best cost value is 2.0 with accuracy = 0.9809745

# 5. Take any other classification method that we
# covered during the class.  Compare it to the svm
# model in terms of accuracy.  Which one does
# better? (7)

## LDA model
set.seed(2016)
LDA_model <- lda(label ~ ., data = train_voice)
LDA_pred <- predict(LDA_model, newdata = test_voice)

confusionMatrix(LDA_pred$class, test_voice$label, positive = "female")

## Results
## Accuracy = 0.9636
## Sensitivity : 0.9589
## Specificity : 0.9684

## Accuracy of LDA model is worse than of SVM radial kernel model: 0.9636 < 0.981

## GLM model
set.seed(2016)
GLM_model <- glm(label ~  ., data = train_voice, family = "binomial")
GLM_pred <- predict(GLM_model, newdata = test_voice, type = "response")

labels <- ifelse(GLM_pred > 0.5, "male", "female")
confusionMatrix(labels, test_voice$label, positive = "female")

## Results
## Accuracy = 0.9699
## Sensitivity : 0.9810
## Specificity : 0.9589

## Accuracy of GLM model is worse than of SVM radial kernel model: 0.9699 < 0.981


# Diabetes dataset contains information about
# different characteristics of the patients, as well as
# if they are diagnosed with diabetes or not.

# 6. Load diabetes.csv into R. Create testing and
# training datasets. As usual, 80% goes to train,
# the rest to test.  The proportions of variable
# Class should be maintained.  Build the lda model
# on the training set. Report the accuracy of the
# model.  Use 1 as positive class for diabetes. (7)

diabetes <- read.csv("Diabetes.csv")
diabetes$Class <- as.factor(diabetes$Class)

set.seed(2016)
index <- createDataPartition(diabetes$Class, p = 0.8, list = FALSE)

train_diab <- diabetes[index,]
test_diab <- diabetes[-index,]

## LDA model
set.seed(2016)
model_lda <- lda(Class ~ . , data = train_diab)
predict_lda <- predict(model_lda, newdata = test_diab)
confusionMatrix(predict_lda$class, test_diab$Class, positive = '1')
## Accuracy = 0.7516

# 7. Using any of the classification methods that
# we covered so far, make predictions for the
# Class. Is that method doing better in comparison
# with LDA in terms of accuracy? (7)

## SVM radial basis kernel model
set.seed(2016)
SVM_radial_model <- svm(Class ~ ., data = train_diab, kernel = "radial", probability = TRUE)
SVM_radial_pred <- predict(SVM_radial_model, newdata = test_diab, probability = TRUE)

confusionMatrix(SVM_radial_pred, test_diab$Class)

## Results
## Accuracy = 0.7908
## Sensitivity : 0.9
## Specificity : 0.5849

## Accuracy of SVM radial model is better than of LDA model: 0.7908 > 0.7516

## GLM model
set.seed(2016)
GLM_model_diab <- glm(Class ~  ., data = train_diab, family = "binomial")
GLM_pred_diab <- predict(GLM_model_diab, newdata = test_diab, type = "response")

labels <- ifelse(GLM_pred_diab > 0.5, "1", "0")
confusionMatrix(labels, test_diab$Class, positive = "1")

## Results
## Accuracy = 0.7647
## Sensitivity : 0.5849
## Specificity : 0.86

## Accuracy of GLM model is also slightly better than of LDA model: 0.7647 > 0.7516

# 8. Bonus 
# Find a dataset. Find something
# interesting related to that data.  Prepare
# predictive methods, do clustering, find
# interesting and meaningful patterns based on that
# data. Do not forget to upload that new data to moodle
# along with your submission. (10)

gender_data <- read.csv("gender_data.csv")

set.seed(2016)
index <- createDataPartition(gender_data$Gender, p = 0.8, list = FALSE)

train_gender <- gender_data[index,]
test_gender <- gender_data[-index,]

## let's classify this data several methods

## LDA model
set.seed(2016)
lda_gender <- lda(Gender ~ . , data = train_gender)
gender_pred_lda <- predict(lda_gender, newdata = test_gender)

confusionMatrix(gender_pred_lda$class, test_gender$Gender, positive = 'Female')
## Accuracy = 0.9195

## SVM polynomial model
set.seed(2016)
svm_gender <- svm(Gender ~ ., data = train_gender, kernel = "polynomial", probability = TRUE)
gender_pred_svm <- predict(svm_gender, newdata = test_gender, probability = TRUE)

confusionMatrix(gender_pred_svm, test_gender$Gender)
## Accuracy = 0.9105

## GLM model
set.seed(2016)
glm_gender <- glm(Gender ~  ., data = train_gender, family = "binomial")
gender_pred_glm <- predict(glm_gender, newdata = test_gender, type = "response")

labels <- ifelse(gender_pred_glm > 0.5, "Male", "Female")
confusionMatrix(labels, test_gender$Gender, positive = "Female")
## Accuracy = 0.9205

## KNN
set.seed(2016)
gender_knn <- knn(train_gender[, 2:3], test_gender[, 2:3], train_gender$Gender, k = 25) # for k = 25

confusionMatrix(gender_knn, test_gender$Gender, positive = "Female")
## Accuracy = 0.914
