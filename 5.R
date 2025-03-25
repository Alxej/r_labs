setwd("D:/r labs/data/")

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
coef(reg3.2)
first_eq <- paste("Частное уравнение для Population - ", P[2] * income_mean + P[1], " +", P[3], " * Population")
first_eq
second_eq <- paste("Частное уравнение для Income - ", P[3] * population_mean + P[1], " +", P[2], " * Income")
second_eq

# Графики для частных коэффициентов эластичности

# Population
x_pop <- M$Population
elastic_1 <- (P[3] * x_pop)/(P[2] * income_mean + P[3] * x_pop)
plot(x=x_pop, y=elastic_1, main="График частного коэффициента эластичности Population")
lines(x_pop[order(x_pop)], elastic_1[order(x_pop)])

# Income
x_inc <- M$Income
elastic_2 <- (P[2] * x_inc)/(P[3] * population_mean + P[2] * x_inc)
plot(x=x_inc, y=elastic_2, main="График частного коэффициента эластичности Income")
lines(x_inc[order(x_inc)], elastic_2[order(x_inc)])

# Фиктивные переменные
ZCFO<-rep(0, times=length(M$FO))
ZUFO<-rep(0, times=length(M$FO))
ZSZFO<-rep(0, times=length(M$FO))
ZUrFO<-rep(0, times=length(M$FO))
ZPFO<-rep(0, times=length(M$FO))
ZSKFO<-rep(0, times=length(M$FO))
ZSFO<-rep(0, times=length(M$FO))
ZDVFO<-rep(0, times=length(M$FO))
ZCFO[M$FO=="CFO"]<-1
ZUFO[M$FO=="UFO"]<-1
ZSZFO[M$FO=="SZFO"]<-1
ZUrFO[M$FO=="UrFO"]<-1
ZPFO[M$FO=="PFO"]<-1
ZSKFO[M$FO=="SKFO"]<-1
ZSFO[M$FO=="SFO"]<-1
ZDVFO[M$FO=="DVFO"]<-1
M1<-data.frame(M,ZCFO,ZUFO,ZSZFO,ZUrFO,ZPFO,ZSKFO,ZSFO)
rm(ZCFO,ZUFO,ZSZFO,ZUrFO,ZPFO,ZSKFO,ZSFO, ZDVFO)

M1

reg4.1<-lm(M1$MilkPrice ~ M1$Income +M1$Population + M1$ZCFO + M1$ZUFO + M1$ZSZFO + M1$ZUrFO + M1$ZPFO + M1$ZSKFO + M1$ZSFO)
summary(reg4.1)
accuracy(reg4.1)
P <- coef(reg4.1)
P
# Фактор Федеральный округ оказывает влияние на Цены на молоко, тк все коэффициенты значимы
# Набольшая цена на молоко в сибирском Ф. округе
# Наименьшая цена в центральном Ф. округе
# Построенную модель можно использовать для прогнозирования

eq_ZCFO <- paste("CFO", " - ", P[1] + P[4], " + ", P[1], " * Income +", P[2], " * Population")
eq_ZCFO
eq_ZUFO <- paste("UFO", " - ", P[1] + P[5], " + ", P[1], " * Income +", P[2], " * Population")
eq_ZUFO
eq_ZSZFO <- paste("SZFO", " - ", P[1] + P[6], " + ", P[1], " * Income +", P[2], " * Population")
eq_ZSZFO
eq_ZUrFO <- paste("UrFO", " - ", P[1] + P[7], " + ", P[1], " * Income +", P[2], " * Population")
eq_ZUrFO
eq_ZPFO <- paste("PFO", " - ", P[1] + P[8], " + ", P[1], " * Income +", P[2], " * Population")
eq_ZPFO
eq_ZSKFO <- paste("SKFO", " - ", P[1] + P[9], " + ", P[1], " * Income +", P[2], " * Population")
eq_ZSKFO
eq_ZSFO <- paste("SFO", " - ", P[1] + P[10], " + ", P[1], " * Income +", P[2], " * Population")
eq_ZSFO
eq_ZDVFO <- paste("DVFO", " - ", P[1], " + ", P[1], " * Income +", P[2], " * Population")
eq_ZDVFO

#Проверка методом Чоу
install.packages("strucchange")  # если не установлен
library(strucchange)
chow_test <- sctest(reg4.1, type = "Chow")
print(chow_test)
# Данные однородны

# Графики
plot(x=M$Income, y=M$MilkPrice, main="Зависимость цены от численности населения" ,xlab = "Среднедушевые доходы", ylab = "Цена на молоко")

points(x=M$Income[M$FO=="CFO"], y=M$MilkPrice[M$FO=="CFO"], col=1)
abline(a=coef(reg4.1)[3] * mean(M$Population[M$FO=="CFO"]) + coef(reg4.1)[1] + coef(reg4.1)[4], b=coef(reg4.1)[2], col=1)

points(x=M$Income[M$FO=="UFO"], y=M$MilkPrice[M$FO=="UFO"], col=2)
abline(a=coef(reg4.1)[3]*mean(M$Population[M$FO=="UFO"])+coef(reg4.1)[1]+coef(reg4.1)[5], b=coef(reg4.1)[2], col=2)

points(x=M$Income[M$FO=="SZFO"], y=M$MilkPrice[M$FO=="SZFO"], col=3)
abline(a=coef(reg4.1)[3]*mean(M$Population[M$FO=="SZFO"])+coef(reg4.1)[1]+coef(reg4.1)[6], b=coef(reg4.1)[2], col=3)

points(x=M$Income[M$FO=="UrFO"], y=M$MilkPrice[M$FO=="UrFO"], col=4)
abline(a=coef(reg4.1)[3]*mean(M$Population[M$FO=="UrFO"])+coef(reg4.1)[1]+coef(reg4.1)[7], b=coef(reg4.1)[2], col=4)

points(x=M$Income[M$FO=="PFO"], y=M$MilkPrice[M$FO=="PFO"], col=5)
abline(a=coef(reg4.1)[3]*mean(M$Population[M$FO=="PFO"])+coef(reg4.1)[1]+coef(reg4.1)[8], b=coef(reg4.1)[2], col=5)

points(x=M$Income[M$FO=="SKFO"], y=M$MilkPrice[M$FO=="SKFO"], col=6)
abline(a=coef(reg4.1)[3]*mean(M$Population[M$FO=="SKFO"])+coef(reg4.1)[1]+coef(reg4.1)[9], b=coef(reg4.1)[2], col=6)

points(x=M$Income[M$FO=="SFO"], y=M$MilkPrice[M$FO=="SFO"], col=7)
abline(a=coef(reg4.1)[3]*mean(M$Population[M$FO=="SFO"])+coef(reg4.1)[1]+coef(reg4.1)[10], b=coef(reg4.1)[2], col=7)

points(x=M$Income[M$FO=="DVFO"], y=M$MilkPrice[M$FO=="DVFO"], col=8)
abline(a=coef(reg4.1)[3]*mean(M$Population[M$FO=="DVFO"])+coef(reg4.1)[1], b=coef(reg4.1)[2], col=8)

legend(10000,83,legend = c("ЦФО","ЮФО","СЗФО","УФО","ПФО","СКФО","СФО","ДВФО"), col = c(1,2,3,4,5,6,7,8), lwd=1, pch = c(1, 1, 1,1,1,1,1,1))
