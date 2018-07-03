census_csv<-read.csv("Census R.csv")
View(census_csv)
str(census_csv)

census_csv$sex<-factor(census_csv$sex, levels = c(1,2), labels = c("Male", "Female"))

summary(census_csv$sex)
str(census_csv)

census_csv$happy<-ordered(census_csv$happy, levels = c(1,2,3), labels = c("Very Happy","Pretty Happy","Not too Happy"), exclude = c(8, 9))
summary(census_csv$happy)


census_csv$age[census_csv$age == 99] <- NA
census_csv$age[census_csv$age == 98] <- NA
census_csv$age[census_csv$age %in% c(98, 99)] <- NA
summary(census_csv$age)
str(census_csv)

mean(census_csv$age, na.rm = TRUE)
median(census_csv$age, na.rm = TRUE)
sd(census_csv$age, na.rm = TRUE)


census_csv1<-census_csv[census_csv$age > 34,]
summary(census_csv1$age)

aggregate(census_csv$age, by=list(census_csv$sex), FUN="mean", na.rm=TRUE)

aggregate(census_csv[,c("age", "sibs")], by=list(census_csv$sex, census_csv$happy), FUN="mean", na.rm=TRUE)


my.table<-table(census_csv$sex, census_csv$happy)
prop.table(my.table)
prop.table(my.table, 1)
prop.table(my.table, 2)







