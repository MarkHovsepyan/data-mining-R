install.packages("neuralnet")
library(neuralnet)
data(infert)
View(infert)
fm<-formula(case~age+parity+induced+spontaneous)
class(fm)
set.seed(2016)
modelnn<-neuralnet(fm, data = infert, hidden =2, linear.output = F, err.fct = "ce", rep=4)
plot(modelnn)
names(modelnn)
modelnn$result.matrix
modelnn$net.result
set.seed(2016)
modelnn<-neuralnet(fm, data = infert, hidden =2, linear.output = F, err.fct = "ce", rep=4  )
plot(modelnn)
modelnn$result.matrix
modelnn$err.fct
modelnn$net.result[2]
cmpt<-compute(modelnn, infert[,c("age","parity", "induced", "spontaneous")], rep = 2)
cmpt$net.result # this are probabilities

train<-read.csv("default train.csv")
test<-read.csv("default test.csv")
paste( names(train[,-9]), collapse="+")
fm3<-(default~age+ed+employ+address+income+debtinc+creddebt+othdebt)
train1<-scale(train)
train1<-as.data.frame(train1)
test1<-scale(test)
test1<-as.data.frame(test1)
Train<-cbind( train1[,-9],default=train[,9])
Test<-cbind(test1[,-9], default=test[,9])
modelnn1<-neuralnet(fm3, data = Train, hidden = 2, rep = 4, linear.output = F, err.fct = "ce")
plot(modelnn1)
modelnn1$result.matrix
cmpt<-compute(modelnn1, Test[,-9], rep = 2)
cmpt$net.result
cmpt
pred1 <-prediction(cmpt$net.result, Test$default)
perf <- performance(pred1,"tpr","fpr")
performance(pred, "auc")@y.values
plot(perf)

Diabetes<-read.csv("Diabetes.csv")

q<-(Diabetes-min(Diabetes$NTS))/(max(Diabetes$Age)-min(Diabetes$NTS))
Diabetes1<-(Diabetes[,-9]-min(Diabetes[,-9]))/(max(Diabetes[,-9])-min(Diabetes[,-9]))
Diabetes<-cbind(Diabetes1,Class=Diabetes[,9])
View(Diabetes)
summary(Diabetes)
NTS<-(Diabetes[,1]-min(Diabetes$NTS))/(max(Diabetes$NTS)-min(Diabetes$NTS))
NTS<-(Diabetes[,2]-min(Diabetes$PGC))/(max(Diabetes$PGC)-min(Diabetes$NTS))
Diabetes<-apply(Diabetes, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
Diabetes<-as.data.frame(Diabetes)
summary(Diabetes)
