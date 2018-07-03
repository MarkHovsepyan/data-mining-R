#T-test
View(College)

t.test(College$Apps ~ College$Private)
t.test(College$Accept ~ College$Private)
t.test(College$Top10perc ~ College$Private)
t.test(College$Personal ~ College$Private)

#Titanic dataset
titanic_csv <- read.csv("Titanic_imputed.csv")
View(titanic_csv)
str(titanic_csv)


#Chi-squared test
titanic_csv$pclass <- factor(titanic_csv$pclass, levels = c(1, 2, 3), labels = c("Rich", "Regular", "Poor"))
titanic_csv$survived <- factor(titanic_csv$survived, levels = c(1, 0), labels = c("Yes", "No"))
#now those are categorical

View(titanic_csv)

#making table
titanic_table1 <- table(titanic_csv$pclass, titanic_csv$survived)
print(titanic_table1)

#applying test
chisq.test(titanic_table1)

#making table
titanic_table2 <- table(titanic_csv$sex, titanic_csv$survived)
print(titanic_table2)

#applying test
chisq.test(titanic_table1)




