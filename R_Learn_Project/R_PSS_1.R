car_data <- read.csv("Vehicle_data.csv")
str(car_data)
View(car_data)

cor(car_data$Power..hp.., car_data$Average.fuel.consumption..l.100.km.)

library(ggplot2)

qplot(Compression.ratio, Power..hp.., data = car_data, colour = Brand)

t.test(car_data$Power..hp.. ~ car_data$Traction.type)

car_table1 <- table(car_data$Brand, car_data$Traction.type)
print(car_table1)

chisq.test(car_table1)
