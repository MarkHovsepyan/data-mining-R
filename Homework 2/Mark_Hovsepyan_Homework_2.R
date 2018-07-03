## importing PISA datasets
pisaTrain <- read.csv("pisa2009train.csv") ## 3663 students
pisaTest <- read.csv("pisa2009test.csv") ## 1570 students

mean(pisaTrain$readingScore) ##497.9114

## function to give a summary of missing values (count na's)
library(plyr)
values_miss <- function(dataframe) lapply(dataframe,function(x)
data.frame(nmiss=sum(is.na(x))))

## calling function on the pisaTrain
values_miss(pisaTrain) ## var's with 0 na's are: grade, male, publicschool, urban, readingscore

?na.omit
pisaTrain <- na.omit(pisaTrain)
pisaTest <- na.omit(pisaTest)

nrow(pisaTrain) ## 2414
nrow(pisaTest) ## 990

## 6 and 7 here

str(pisaTrain)
str(pisaTest)

?relevel
pisaTrain$raceeth <- relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth <- relevel(pisaTest$raceeth, "White")

## creating a model with readingScore as a dependent variable
pisa_model <- lm(readingScore~., data = pisaTrain)

summary(pisa_model)

library(memisc)
mtable(pisa_model)

## prediction training
pisa_predict <- predict(pisa_model, newdata = pisaTrain)
## RMSE
RMSE_train_pisa <- sqrt( mean( ( pisaTrain$readingScore - pisa_predict )^2 ) )
RMSE_train_pisa ## 73.36555

## prediction testing
pisa_predict <- predict(pisa_model, newdata = pisaTest)
## RMSE
RMSE_test_pisa <- sqrt( mean( ( pisaTest$readingScore - pisa_predict )^2 ) )
RMSE_test_pisa ## 76.29079

## problem 11
levels(pisaTrain$raceeth)
str(pisaTrain)

## problem 13
summary(pisa_model) ## take alfa = 0.05

## problem 14
pisa_baseline <- mean(pisaTrain$readingScore)
pisa_baseline

## problem 15

pisa_SST = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
pisa_SST

## problem 16



## importing Flu dataset
flu <- read.csv("Flu.csv")
## problem 17
hist(Flu$ILI, xlab="ILI", ylab="Frequemcy") 
## when ILI goes down, the frequencies are decreasing
summary(flu$ILI)

## problem 18
library(ggplot2)
cor(log(flu$ILI), flu$Queries)
qplot(log(ILI), Queries, data = flu)

## problem 19

flu_qplot <- qplot(log(flu$ILI), flu$Queries)
flu_qplot + geom_abline(intercept=-2, slope=1)

## problem 20

FluTrend1 <- lm(log(flu$ILI) ~ flu$Queries)
summary(FluTrend1)

##problem 21
set.seed(20)
sub <- sample(nrow(flu), floor(nrow(flu)*0.75))
fluTrain <- flu[sub, ]
fluTest <- flu[-sub,]

flu_model <- lm(ILI ~ Queries,data = fluTrain)
flu_pred <- predict(flu_model, newdata = fluTrain)
flu_RMSE <- sqrt( mean( ( fluTrain$ILI - pisa_predict )^2 ) )

## problem 22

flu_baseline <- sqrt( mean ( ( fluTest$ILI - mean( fluTrain$ILI ) )^2) )
flu_baseline

## importing climate_change dataset
climate_change <- read.csv("climate_change.csv")

## creating train and test datasets
?subset
climateTrain <- subset(climate_change, Year <= 2006)
climateTest <- climate_change[!(climate_change$Year %in% climateTrain$Year),]

## problem 23
climate_fm <- formula(Temp ~ MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols-Year-Month)

climate_model <- lm(climate_fm, data = climateTrain)

summary(climate_model)


## problem 24
cor(climateTrain) #correlations between all variables

new_climate_fm <- formula(Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols)
new_climate_model <- lm(new_climate_fm, data = climateTrain)
summary(new_climate_model)

cor( climateTrain )
model_clim1 <- lm( Temp~MEI+N2O+TSI+Aerosols, data = climateTrain )
summary(model_clim1) # 0.02532 and Adj. R-squared:  0.7222 


## problems 30 and 31
library(MASS)

climate_model1 <- lm(Temp~.-Year-Month, data = climateTrain)
climate_step <- stepAIC(climate_modelAIC, direction = 'both')
climate_step$anova

climate_stepModel <- lm(Temp~.-Year-Month-CH4, data = climateTrain)
summary(climate_stepModel) #Adj. R-squared:  0.7261

step_pred <- predict(climate_stepModel, newdata = climateTest)
RMSE_step <- sqrt( mean( ( climateTest$Temp - step_pred )^2 ) )
RMSE_step # 0.09522876
