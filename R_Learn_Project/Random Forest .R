#install.packages("randomForest")
library(lattice)
library(ggplot2)
library(e1071)
library(caret)
library(ROCR)
library(caTools)
library(class)
library(randomForest)

PML<-read.csv("pml.csv")
str(PML)
set.seed(2016)
TrainIndex<-createDataPartition(PML$classe, p = 0.8, list = FALSE)
Train<-PML[TrainIndex,]
Test<-PML[-TrainIndex,]

set.seed(2016)
Model<-randomForest(classe~., data = Train, ntree = 50)
Model
#What's inside the model
names(Model)
Model$err.rate

plot(Model)

PredClass<-predict(Model, newdata = Test)
PredProb<-predict(Model, newdata = Test, type = "prob")
Pred<-predict(Model, newdata = Test, type = "vote", norm.votes = FALSE)
identical(50*PredProb, Pred) #most probably a rounding error
identical(PredProb, Pred/50)

set.seed(2016)
Model1<-randomForest(classe~., data=Train, do.trace = TRUE, importance = TRUE, ntree = 100)
#dotrace shows how the accuracy level changes (visualizes the outcome)
varImpPlot(Model1)


Diabetes<-read.csv("Diabetes.csv")
Diabetes$Class<-factor(Diabetes$Class, levels = c(0,1), labels = c("No","Yes"))
Model2<-randomForest(Class~., data = Diabetes, do.trace =TRUE, importance = TRUE, ntree = 500, mtry = 4, nodesize = 60, maxnodes = 15)
plot(Model2)

library(caret)
Control<-trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE)
Grid<-expand.grid(mtry = c(3,4,5,6,7))
set.seed(2016)
Fit<-train(Class~., data = Diabetes, method = "rf", trControl = Control, tuneGrid = Grid, metric = "ROC")
names(Fit)
Fit$metric
plot(Fit)
