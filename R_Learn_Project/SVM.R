library(e1071)
data("iris")

iris1 <- iris[iris$Species != 'virginica', ]
iris1$Species <- factor(iris1$Species)

svm_model1 <- svm(Species~Sepal.Width + Sepal.Length, 
                  data = iris1, kernel = "linear")
 
plot(svm_model1, data = iris1, Sepal.Length~Sepal.Width)

svm_model2 <- svm(Species~Sepal.Width + Sepal.Length, 
                  data = iris1, kernel = "polynomial", probability = TRUE)

pred <- predict(svm_model2, data = iris1, probability = TRUE)
pred

plot(svm_model2, data = iris1, Sepal.Length~Sepal.Width)
