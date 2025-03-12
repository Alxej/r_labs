setwd("D:/r labs/data/")
T<-read.table("reg.txt", header=TRUE)
second_var = "A12"
T<-T[order(T$A1),]
reg<-lm(formula=T[[second_var]] ~T$A1)
summary(reg)
format(coef(reg), digits = 6)
plot(x=T$A1, y=T[[second_var]])
abline(reg, col="red") 
mean(reg$residuals)
plot(reg$residuals, main="График остатков", xlab="Номер наблюдения", ylab="Остатки")
abline(h=0)
library("forecast") 
accuracy(reg)
T<-T[1,]
T$A1<- 1350
format(predict(reg, newdata=T, interval="confidence", level=0.9), digits=10)
