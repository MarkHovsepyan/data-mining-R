# Decision trees
# The dataset voting contains information about the voting
# patterns of Democrat and Republican parties in US Congress.
# You can find more information in the description file.

## libraries that may be needed
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
library(ROCR)
library(caTools)

#1. Load the voting_test and voting_train datasets into R.
# Get rid of the spaces and other symbols in the column 
# names using function make.names. You should get names like
# this: el.salvador.aid. Look at the help of the function
# for the details. (6.25)

load("voting_train.rda")
load("voting_test.rda")

colnames(v_train) <- make.names(colnames(v_train), unique = FALSE, allow_ = TRUE)
colnames(v_test) <- make.names(colnames(v_test), unique = FALSE, allow_ = TRUE)

#2. Create a decision tree with the library rpart on the 
# training set. Use the variable Class as dependent
#variable and all other variables as independent. (6.25)

dtree <- rpart(Class~., data = v_train, method = "class")
prp(dtree, 
    type = 1, #type of the plot
    extra = 4, #display extra information 
    faclen = 5,# length of the factor variable to be shown 
    main = "Decision tree for voting data") 

#3. Plot the decision tree with the library prp and 
# rattle. Make sure you are getting clear and legible 
# plot without overlapping nodes. (6.25)

fancyRpartPlot(dtree)

#4. Make prediction on the testing set. Report the accuracy
# of your model. Take republican as a positive class. (6.25)

pred_class <- predict(dtree, v_test, type = "class")
## confusion matrix
table(Actual = v_test$Class, Predicted = pred_class)
confusionMatrix(pred_class, v_test$Class, positive = "republican")

#5. Now start playing with different parameters of your
# tree (cp, minbucket, minsplit). Your goal is to get a 
# better model than the first one, i.e. the accuracy
# of your model should be higher than the initial one. (6.25)

dtree_new <- rpart(Class~., data = v_train, method = "class", 
                   minsplit = 20, 
                   minbucket = 15, 
                   cp = 0.05)
prp(dtree_new, 
    type = 1, #type of the plot
    extra = 4, #display extra information 
    faclen = 5,# length of the factor variable to be shown 
    main = "Decision tree for voting data") 

pred_class_new <- predict(dtree_new, v_test, type = "class")

## confusion matrix
confusionMatrix(pred_class_new, v_test$Class, positive = "republican")

#6. Is the second model doing better job in predicting
# affiliation for democratic or republican party? Explain.
# (6.25)

## Our goal was to increase accuracy. Now it is 0.8333 that is higher than in the previous model.
## Sensitivity of the old model is 0.7619, whereas the one of new model is 0.7381.
## Specificity of the old model is 0.8333, whereas the one of new model is 0.8939. 
## Here the results are controversial. However, our goal is achieved, that is accuracy is increased.

#7. What are the rules to classify a congressmen as a 
#democrat? (6.25)

asRules(dtree_new)
## Rule 3: adoption.of.the.budget.resolution variable having value "n" means
## that their probablity of voting for a republican is 0.83. 
## Rule 2: adoption.of.the.budget.resolution variable having value "y"/"un" means
## that their probability of voting for a democrat is 0.92.

#8. Using your last model, plot the ROC and calculate
# the area under the curve. Use republican as the class
# of interest. (6.25)

pred_vote <- predict(dtree_new, v_test, type = "prob")
prediction_vote <- prediction(pred_vote[, 2], v_test$Class)
perf <- performance(prediction_vote, "tpr", "fpr")
performance(prediction_vote, "auc")@y.values #AUC = 0.8160173
plot(perf)

#9.BONUS! (10)
# Using library caret, come up with the most optimal
# value of cp to use it in the decision tree for ebay 
# problem. You need to do cross validation 
# in order to find that value. We had this kind of problem
# in KNN homework. Report your findings.

set.seed(2016)
ctrl_1 <- trainControl(method="repeatedcv", number = 15, repeats = 5, 
                    classProbs = TRUE, 
                    summaryFunction = twoClassSummary)

set.seed(2016)
fit_vote <- train(Class~., data = v_train, method = "rpart",
                  preProcess = c("center", "scale"),
                  trControl = ctrl_1, 
                  tuneLength=50)

fit_vote$results
fit_vote$bestTune
plot(fit_vote)

## As we can see the best cp is 0.
## We can conclude that with this cp no other variable is needed. (cp is responsible for the complexity of the tree)
