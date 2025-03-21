cityname <- c("Petrozavodsk", "New-York", "Penza", "London", "Bogota")
people <- c(235000, 7931000, 488000, 9748000, 8034000)
dist <- c(1023, 7512, 555, 2496, 10901)
salary <- c(90780.00, 685050.00, 54770.33, 230300.92, 31070.35)

data <- data.frame(cityname, people, dist, salary)
data

setwd("C:/Users/Alxej/Documents")
tab <- read.table("Cities.txt", header=TRUE)
tab$people
plot(x=tab$dist, y=tab$salary, xlab="Расстояние до Москвы", ylab="Средняя заработная плата", main="Диаграмма рассеяния", col="red", pch=2)
text(x=tab$dist, y=tab$salary-10, labels=tab$cityname) # вывод подписей к точкам

newyork_data <- tab[tab$cityname == "New-York",]
newyork_data
penza_data <- tab[tab$cityname == "Penza",]
d <- (newyork_data$salary - penza_data$salary) / (newyork_data$dist - penza_data$dist)
d
c <- penza_data$salary - d * penza_data$dist
c
abline(a=c, b=d, col="green")
