setwd("C:/xd/r_labs/data/")

M<-read.csv2("Milk.csv", header=TRUE, sep=";" , dec = ",")
View(M)
cor(data.frame(M$MilkPrice,M$Income,M$Population))
plot(x=M$Income,y=M$MilkPrice, main="Зависимость цены от дохода")
M<-M[order(M$Income),]
reg1<-lm(M$MilkPrice~M$Income)
summary(reg1)
abline(reg1)
plot(residuals(reg1))
abline(h=0, col="red")
library("forecast")
accuracy(reg1)

plot(x=M$Population,y=M$MilkPrice, main="Зависимость цены от численности населения" ,
     xlab = "Среднедушевые доходы", ylab = "Цена на молоко")
M<-M[order(M$Population),]
reg2<-lm(M$MilkPrice~M$Population)
summary(reg2)
abline(reg2)
plot(residuals(reg2))
abline(h=0, col="red")
accuracy(reg2)

reg3<-lm(M$MilkPrice~M$Income+M$Population)
summary(reg3)
accuracy(reg3)
plot(residuals(reg3))
abline(h=0, col="red")

M<-M[order(M$Income),]
reg3.1<-lm(M$MilkPrice~M$Income+M$Population)
M<-M[order(M$Income),]
summary(reg3.1)
accuracy(reg3.1)
plot(residuals(reg3.1))
abline(h=0, col="red")

M<-M[order(M$Population),]
reg3.2<-lm(M$MilkPrice~M$Income+M$Population)
M<-M[order(M$Income),]
summary(reg3.2)
accuracy(reg3.2)
plot(residuals(reg3.2))
abline(h=0, col="red")

# Построение частных уравнений регрессии

M

population_mean <- mean(M$Population)
income_mean <- mean(M$Income)

P <- coef(reg3.2)
P[2]
first_eq <- paste("Частное уравнение для Population - ", P[2] * income_mean, " +", P[3], " * Population")
first_eq
second_eq <- paste("Частное уравнение для Income - ", P[3] * population_mean, " +", P[2], " * Income")
second_eq
