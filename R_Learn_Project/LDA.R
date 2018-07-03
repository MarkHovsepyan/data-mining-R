library(ggplot2)
library(MASS)
library(klaR)
library(caret)


data(iris)

iris1 <- iris[iris$Species != 'virginica', ]
table(iris1$Species)
iris1$Species <- factor(iris1$Species)

qplot(Sepal.Width, Sepal.Length, data = iris1, colour = Species,
      main = "Scatterplot of Sepal Length and Sepal Width")+
  geom_abline(intercept = 2.8, slope = 0.8, size = 3)


iris2 <- iris[iris$Species != 'versicolor', ]
iris2$Species <- factor(iris2$Species)

qplot(Sepal.Width, Sepal.Length, data = iris2, colour = Species,
      main = "Scatterplot of Sepal Length and Sepal Width")+
  geom_abline(intercept = 2.8, slope = 0.8, size = 3)

model <- lda(Species~., data = iris)
model

X <- predict(model)
table (iris$Species, X$class)

qplot(X$x[,1], X$x[,2], colour = iris$Species)

ldahist(X$x[,1], iris$Species)
ldahist(X$x[,2], iris$Species)

partimat(Species~., data = iris, method = 'lda')


## Wine prediction
wine <- read.csv("wine.csv")
wine$Type <- factor(wine$Type)
set.seed(1973)
index <- createDataPartition(wine$Type, p = 0.7, list = FALSE)
train <- wine[index,]
test <- wine[-index,]

model <- lda(Type~., data = train)
model

pred <- predict(model, newdata = test)
table(wine$Type, pred$class)


LD1 <- 

qplot(pred$x[,1], pred$x[,2], colour = test$Type) + 
  geom_abline(slope = 0.6589, size = 1) +
  geom_abline(slope = 0.3411, size = 1)

ldahist(LD1)


