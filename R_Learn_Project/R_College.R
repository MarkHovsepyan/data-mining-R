load("College.rda")
str(College)
View(College)
summary(College)

rownames(College)<-College$University
College$University<-NULL

cor(College$Apps, College$Accept)
cor(College$Top10perc, College$Accept)

plot(College$Apps, College$Accept)
pairs(~Apps+Accept+Enroll, data = College)

cor(College[,c("Apps", "Accept", "Enroll")])
cor(College[,-1])
print(cor(College[,-1]), digits = 2)

cor(College$Accept, College$Top10perc)
pairs(~Apps+Accept+Top10perc, data = College)

cor(College[,c("Apps","Room.Board","Books","Personal", "PhD", "Terminal", "S.F.Ratio")])




