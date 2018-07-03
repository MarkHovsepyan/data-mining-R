set.seed(1313)

sub <- sample(nrow(abalone_csv), floor(nrow(abalone_csv)*0.7))

sub[1:50]

Train <- abalone_csv[sub, ]
Test <- abalone_csv[-sub,]


cor(Train[-c(1, 2, 3, 4, 5, 7, 8)])

model1 <- lm(rings ~ sex+height+weight.w+weight.sh, data = Train)

model2 <- lm(rings ~ sex+height+weight.sh, data = Train)

model3 <- lm(rings ~ sex+height+weight.w, data = Train)

library(memisc)
mtable(model1, model2, model3)

RMSE_base <- sqrt( mean( ( Test$rings - mean( Train$rings ) )^2 ) )
RMSE_base

pred1 <- predict(model1, newdata = Test)
pred2 <- predict(model2, newdata = Test)
pred3 <- predict(model3, newdata = Test)

RMSE_1 <- sqrt( mean( ( Test$rings - pred1 )^2 ) )
RMSE_2 <- sqrt( mean( ( Test$rings - pred2 )^2 ) )
RMSE_3 <- sqrt( mean( ( Test$rings - pred3 )^2 ) )

RMSE_1
RMSE_2
RMSE_3


boston_csv <- read.csv("Boston.csv")
View(boston_csv)

library(ggplot2)

cor(boston_csv[-c(4)])

qplot(log(DIS), MEDV, data = boston_csv)
qplot(log(CRIM), MEDV, data = boston_csv) #nope
qplot(ZN, MEDV, data = boston_csv) #nope
qplot(RM, MEDV, data = boston_csv) #linear relation
qplot(AGE, MEDV, data = boston_csv) #
qplot(sqrt(DIS), MEDV, data = boston_csv) #
qplot(1/LSTAT, MEDV, data = boston_csv)

qplot(log(LSTAT), MEDV, data = boston_csv)

cor(boston_csv$LSTAT^2, boston_csv$MEDV)
cor(log(boston_csv$LSTAT), boston_csv$MEDV)
cor(1/boston_csv$LSTAT, boston_csv$MEDV)
cor(1/log(boston_csv$LSTAT), boston_csv$MEDV)

boston_csv$LOG_STAT <- log(boston_csv$LSTAT)

set.seed(2020)

sub <- sample(nrow(boston_csv), floor(nrow(boston_csv)*0.7))

Train <- boston_csv[sub, ]
Test <- boston_csv[-sub,]

#formulas
fm1 <- formula(MEDV~CRIM+ZN+INDUS+CHAS+NOX-RM+AGE+DIS+TAX+PTRATIO+B+LOG_STAT)

fm2 <- formula(MEDV~CRIM+ZN+INDUS+CHAS+NOX-RM+AGE+DIS+RAD+PTRATIO+B+LOG_STAT)

fm3 <- formula(MEDV~CRIM+ZN+INDUS+CHAS+NOX-RM+AGE+DIS+TAX+PTRATIO+B+LSTAT)

#creating model

model1 <- lm(fm1, data = Train)
model2 <- lm(fm2, data = Train)
model3 <- lm(fm3, data = Train)


pred1 <- predict(model1, newdata = Test)
pred2 <- predict(model2, newdata = Test)
pred3 <- predict(model3, newdata = Test)

RMSE_1 <- sqrt( mean( ( Test$MEDV - pred1 )^2 ) )
RMSE_2 <- sqrt( mean( ( Test$MEDV - pred2 )^2 ) )
RMSE_3 <- sqrt( mean( ( Test$MEDV - pred3 )^2 ) )

RMSE_1; RMSE_2; RMSE_3

summary(model2)

new_fm <- formula(MEDV~CRIM-ZN-INDUS+CHAS+NOX-RM+AGE+DIS+RAD+PTRATIO+B+LOG_STAT)

new_model <- lm(new_fm, data = Train)

new_pred <- predict(new_model, newdata = Test)

new_RMSE <- sqrt( mean( ( Test$MEDV - new_pred )^2 ) )

new_RMSE #worse RMSE cause of removed variables (even not significant)


model_all <- lm(MEDV~.-LSTAT, data = Train)

library(MASS)

step <- stepAIC(model_all, direction = "both")

step$anova




