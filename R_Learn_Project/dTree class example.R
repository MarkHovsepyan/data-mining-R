library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
library(ROCR)

###Titanic Example###
Titanic<-read.csv("Titanic_imputed.csv", nrows=1310)

Titanic$pclass<-factor(Titanic$pclass, levels=c(1,2,3), labels=c("1st", "2nd", "3rd"))
Titanic$survived<-factor(Titanic$survived, levels=c(0,1), labels=c("No", "Yes"))


###Create training and testing datasets

set.seed(2015)
trainIndex <- createDataPartition(Titanic$survived, p = .75, list = FALSE)                
Train<-Titanic[trainIndex,]
Test<-Titanic[-trainIndex,]

###contingency table and conditional probabilities

#The decision tree with only gender as an independent variable
fit<-rpart(survived~sex, data=Titanic, method="class")
prp(fit, 
    type=1, #type of the plot
    extra=4, #display extra information 
    faclen=5,# lenght of the factor variable to be shown 
    main="Decision tree for titanic") 

# from rattle
fancyRpartPlot(fit)

###Lets have a look at the contingency table
prop.table(table(Titanic$sex,Titanic$survived),1)

##Lets try with the pclass
fit<-rpart(survived~pclass, data=Titanic, method="class")
prp(fit, type=1, extra=4, faclen=5, main="Decision tree for titanic") 
prop.table(table(Titanic$pclass,Titanic$survived),1)



fit<-rpart(survived~sex+age+sibsp+parch+pclass, 
           data=Train, method="class", minsplit=25)
prp(fit, type=1, extra=1, faclen=0, tweak=0.5) #lenght of factor level names
                                                #tweak text size
fancyRpartPlot(fit, tweak=1.1)

asRules(fit) #rattle

###Predict the class
pred_class<-predict(fit, Test, type="class")
###Confusion Matrix, Lets assume that "Yes" is our positive instance
table(Actual=Test$survived, Predicted=pred_class)


######Do it yourself#####
weather<-read.csv("weather.csv")
summary(weather)


###testing and training datasets
set.seed(1982)

trainIndex <- createDataPartition(weather$RainToday, p = .75, list = FALSE)                
Train<-weather[trainIndex,]
Test<-weather[-trainIndex,]

fit<-rpart(RainTomorrow~.-Rainfall, data=Train, method="class")
prp(fit, type=1, extra=4)
asRules(fit) #rattle
#printcp(fit) 
fancyRpartPlot(fit, tweak=1.2)

##Making predictions##
pred_class<-predict(fit, Test, type="class")
###Confusion Matrix, Lets assume that "Yes" is our positive instance
table(Actual=Test$RainToday, Predicted=pred_class)
### Create confusion matrix
confusionMatrix(pred_class, Test$RainToday, positive = "Yes")
# ROC curve
pred_prob <- predict(fit, Test, type="prob")

pred <- prediction(pred_prob[,2], Test$RainTomorrow) 
perf <- performance(pred,"tpr","fpr")

performance(pred, "auc")@y.values

plot(perf)

# tunning the rpart tree

set.seed(2015)
fit<-rpart(RainTomorrow~.-Rainfall, data=Train, 
            method="class", 
            # minsplit=25, # minimum number of cases to be in the node
            # minbucket=15 # the minimum number of observations in any terminal <leaf> node
            cp=0.01)
prp(fit)
fit
fancyRpartPlot(fit)

