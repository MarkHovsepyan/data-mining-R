#Getting the working directory
getwd()
data(mtcars)
str(mtcars)
View(mtcars)
dim(mtcars)
nrow(mtcars)
ncol(mtcars)
dim(mtcars)[1]
dim(mtcars)[2]
?ncol

mtcars
head(mtcars, n = 5)
tail(mtcars, n = 4)


mtcars[5:10,1:3]

mtcars[,1]
mtcars[,"mpg"]
mtcars$mpg

a<-c(1,2,3)
a
mean(x=c(2,5,6))
x
mean(x<-c(2,5,6))
x
rm(x)

new_df<-mtcars[,c(1:3,7)]
names(mtcars)
View(new_df)
new_df1<-mtcars[,c("mpg", "hp", "wt")]
vec = c("mpg", "hp", "wt")
str(new_df1)


min(mtcars$mpg)
which.min(mtcars$mpg)
mtcars[15,]

max(mtcars$hp)
which.max(mtcars$hp)
mtcars[31,]

rr<-rownames(mtcars)
max(mtcars$wt)
which.max(mtcars$wt)
rr[16]

rownames(mtcars)[which.max(mtcars$wt)]

vector_1 <- c(1:25,50)
vector_1
mean(vector_1)
median(vector_1)
sd(vector_1)
range(vector_1)
range(vector_1)[2] - range(vector_1)[1]


summary(mtcars)






