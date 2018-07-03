blood <- read.table("blood pressure.txt", header = TRUE)
View(blood)
str(blood)

blood$Index <- NULL
blood$One <- NULL

library(ggplot2)

qplot(blood$Age, blood$Pressure)

model <- lm(Pressure~Age, data = blood)
summary(m_blood)

#calculating accordind to lm results (age = 18)
98.7147 + 0.9709*18


qplot(blood$Age, blood$Pressure) + geom_abline(intercept = 98.7, slope = 0.9709)

which.max(blood$Pressure)

blood1 <- blood[-which.max(blood$Pressure),]

model1 <- lm(Pressure~Age, data = blood1)
summary(model1)

qplot(blood1$Age, blood1$Pressure) + geom_abline(intercept = 97.0771, slope = 0.9493) + geom_abline(intercept = 98.7, slope = 0.9709)

summary(model1)
names(model1)

model1$residuals

hist(model1$residuals)
qplot(model1$residuals, geom = "histogram")
qplot(model1$residuals, geom = "histogram")

#Abalone problem

abalone_csv <- read.csv("Abalone.csv")
View(abalone_csv)

boxplot(rings~sex, data = abalone_csv)

abalone_csv$sex <- as.character(abalone_csv$sex)
abalone_csv$sex[abalone_csv$sex != 'I'] <- 'NI' #NI means non-infant
abalone_csv$sex <- as.factor(abalone_csv$sex)

table(abalone_csv$sex)
boxplot(rings~sex, data = abalone_csv)

cor(abalone_csv[,-c(1)])


model1 <- lm(rings~.,data = abalone_csv)
summary(model1)

model2 <- lm(rings~. -length, data = abalone_csv)
summary(model2)
