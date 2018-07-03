census_csv <- read.csv("Census R.csv")


census_csv$sex <- factor(census_csv$sex, levels = c(1, 2), labels = c("Male","Female"))

my.table<-table(census_csv$sex)

barplot(my.table, ylab = "Frequency", xlab = "Gender", 
        main = "Sex Frequency Distribution", legend.text = c("Male", "Female"))


str(mtcars)


plot(mtcars$mpg, mtcars$hp, ylab = "Horse Power", xlab = "Miles Per Hour", 
     main = "Scatterplot for MPG and HP")



hist(census_csv$age, ylab = "Frequency", xlab = "Age", 
     main = "Histogram for Age", freq = FALSE)

library(ggplot2)

data(diamonds)

str(diamonds)

set.seed(1313)

dsmall <- diamonds[sample(nrow(diamonds), 400),]

View(dsmall)

str(dsmall)

qplot(log(carat), price, data = dsmall)


qplot(carat, price, data = dsmall, colour = clarity)

qplot(carat, price, data = dsmall, shape = clarity, colour = color)


View(dsmall)


qplot(color, price/carat, data=diamonds, color = color, geom = "boxplot")

qplot(color, price/carat, data=diamonds, color = color, geom = "jitter")

qplot(carat, data=diamonds, color = color, geom = "histogram", binwidth = 0.1,
      xlim = c(0, 3))


qplot(carat, data=diamonds, geom = "density")

qplot(carat, data=diamonds, geom = "histogram", fill = color, binwidth = 0.1)

qplot(color, data = diamonds, geom = "bar")

qplot(carat, data = diamonds, geom = "histogram", facets = color~., 
      xlim = c(0, 3), binwidth = 0.1)






