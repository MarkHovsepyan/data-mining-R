load("voting_train.rda") # train set
load("voting_test.rda") # test set

library(e1071)
library(caret)
library(caTools)
library(ROCR)

model <- naiveBayes(Class~., data = Train, laplace = 1)
summary(model)
names(model)

model$apriori # class frequencies
model$tables  # conditional probabilities
model$levels
model$call

# making predicitions
pred_test <- predict(model, newdata = Test)
confusionMatrix(pred_test, Test$Class) # confusion matrix

pred_test_prob <- predict(model, newdata = Test, type = "raw")

p_test1 <- prediction(pred_test_prob[, 1], Test$Class, label.ordering = c("republican", "democrat"))
p_test2 <- prediction(pred_test_prob[, 2], Test$Class, label.ordering = c("democrat", "republican"))

# for positive = democrat
perf <- performance(p_test1, "tpr", "fpr")
plot(perf)
performance(p_test1, "auc")@y.values

# for positive = republican
perf <- performance(p_test2, "tpr", "fpr")
plot(perf)
performance(p_test2, "auc")@y.values


## SPAM

spam <- read.csv("spam.csv")
spam$is_spam <- factor(spam$is_spam, levels = c(0, 1), labels = c("No", "Yes"))
is_spam <- spam$is_spam

set.seed(2016)
index <- createDataPartition(spam$is_spam, p = .8, list = FALSE)                
Train <- spam[index,]
Test <- spam[-index,]

model_spam <- naiveBayes(is_spam~., data = Train, laplace = 1)

pred_test_spam <- predict(model_spam, newdata = Test)
confusionMatrix(pred_test_spam, Test$is_spam, positive = "Yes")

pred_test_prob_spam <- predict(model_spam, newdata = Test, type = "raw")

p_test_spam <- prediction(pred_test_prob_spam[, 1], Test$is_spam, label.ordering = c("Yes", "No"))

perf <- performance(p_test_spam, "tpr", "fpr")
plot(perf)
performance(p_test_spam, "auc")@y.values


