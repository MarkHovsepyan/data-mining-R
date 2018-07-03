Titanic <- read.csv("titanic_imputed.csv")

Titanic$survived <- factor(Titanic$survived, levels = c(1, 0), labels = c("Yes", "No"))

## first model

model <- glm(survived~sex, data = Titanic, family = "binomial")

summary(model)

exp(model$coefficients)

table(Titanic$survived, Titanic$sex)

## calculating probabilities
161/(682 + 161) # males survived
682/(682 + 161) # males not survived

339/(127 + 339) # females survived
127/(127 + 339) # females not survived

161/682 # odds for males to survive
339/127 # odds for females to survive

## creating test and train using caret
library(caret)

set.seed(1333)

trainindex <- createDataPartition(Titanic$survived, p= .75, list = FALSE) # survived is dependent variable

Train <- Titanic[trainindex,]
Test <- Titanic[-trainindex,]

model_s <- glm(survived~sex+pclass+age+sibsp, data = Train, family = "binomial")

summary(model_s)

predTest <- predict(model_s, newdata = Test, type = "response")

summary(predTest)
predTest

table(predTest > 0.5, Test$survived)

(168 + 89) / (168 + 44 + 26 + 89) # accuracy is 0.7859327
89 / (89 + 44) # sensitivity
44 / (44 + 168) # wrong

pr_label <- ifelse(predTest > 0.5, "Yes", "No")
pr_label

??confusionMatrix

confusionMatrix(pr_label, Test$survived, positive = "Yes")

## create ROC curve
## create prediction

library(ROCR)

p_test <- prediction(predTest, Test$survived)

perf <- performance(p_test, "tpr", "fpr")
plot(perf)



