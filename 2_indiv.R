# install.packages("openxlsx", dependencies = TRUE)
library(openxlsx)
library("forecast")

setwd("C:/xd/r_labs/data/")

M<-read.xlsx("sugar_prices.xlsx", sheet=1, startRow = 1,)
M<-M[order(M$Price),]

ZCFO<-rep(0, times=length(M$Fed))
ZUFO<-rep(0, times=length(M$Fed))
ZSZFO<-rep(0, times=length(M$Fed))
ZUrFO<-rep(0, times=length(M$Fed))
ZPFO<-rep(0, times=length(M$Fed))
ZSKFO<-rep(0, times=length(M$Fed))
ZSFO<-rep(0, times=length(M$Fed))
ZDVFO<-rep(0, times=length(M$Fed))

# Федеральный округ влияет только на свободный член
ZCFO[M$Fed=="cfo"]<-1
ZUFO[M$Fed=="ufo"]<-1
ZSZFO[M$Fed=="szfo"]<-1
ZUrFO[M$Fed=="urfo"]<-1
ZPFO[M$Fed=="pfo"]<-1
ZSKFO[M$Fed=="skfo"]<-1
ZSFO[M$Fed=="sfo"]<-1
ZDVFO[M$Fed=="dfo"]<-1
M1<-data.frame(M,ZCFO,ZDVFO,ZSZFO,ZUrFO,ZPFO,ZSKFO,ZSFO)
rm(ZCFO,ZUFO,ZSZFO,ZUrFO,ZPFO,ZSKFO,ZSFO, ZDVFO)

ICFO<-rep(0, times=length(M$Fed))
IUFO<-rep(0, times=length(M$Fed))
ISZFO<-rep(0, times=length(M$Fed))
IUrFO<-rep(0, times=length(M$Fed))
IPFO<-rep(0, times=length(M$Fed))
ISKFO<-rep(0, times=length(M$Fed))
ISFO<-rep(0, times=length(M$Fed))
IDVFO<-rep(0, times=length(M$Fed))

ICFO[M$Fed=="cfo"]<-M$Income[M$Fed=="cfo"]
IUFO[M$Fed=="ufo"]<-M$Income[M$Fed=="ufo"]
ISZFO[M$Fed=="szfo"]<-M$Income[M$Fed=="szfo"]
IUrFO[M$Fed=="urfo"]<-M$Income[M$Fed=="urfo"]
IPFO[M$Fed=="pfo"]<-M$Income[M$Fed=="pfo"]
ISKFO[M$Fed=="skfo"]<-M$Income[M$Fed=="skfo"]
ISFO[M$Fed=="sfo"]<-M$Income[M$Fed=="sfo"]
IDVFO[M$Fed=="dfo"]<-M$Income[M$Fed=="dfo"]
M1<-data.frame(M1,ICFO,IUFO,ISZFO,IUrFO,IPFO,ISKFO,ISFO)
rm(ICFO,IUFO,ISZFO,IUrFO,IPFO,ISKFO,ISFO, IDVFO)

reg4.1<-lm(M1$Price ~ M1$Income  + M1$Shirota + M1$Population + 
             M1$ZCFO + M1$ZDVFO + M1$ZSZFO + M1$ZUrFO + M1$ZPFO + M1$ZSKFO + M1$ZSFO +
             M1$ICFO + M1$IUFO + M1$ISZFO + M1$IUrFO + M1$IPFO + M1$ISKFO + M1$ISFO)
summary(reg4.1)
accuracy(reg4.1)
P <- coef(reg4.1)
P

#Проверка методом Чоу
# install.packages("strucchange")  # если не установлен
library(strucchange)
chow_test <- sctest(reg4.1, type = "Chow")
print(chow_test)
# Выборка содержит структурные изменения

# Графики
plot(x=M$Income, y=M$Price, main="Зависимость цены от доходов" ,xlab = "Среднедушевые доходы", ylab = "Цена на сахарный песок")

points(x=M$Income[M$Fed=="cfo"], y=M$Price[M$Fed=="cfo"], col=1)
abline(a= P[3] * mean(M$Shirota[M$Fed=="cfo"]) + P[4] * mean(M$Population[M$Fed=="cfo"]) + P[1] + P[5], b=P[2] + P[12], col=1)

points(x=M$Income[M$Fed=="ufo"], y=M$Price[M$Fed=="ufo"], col=2)
abline(a=P[3] * mean(M$Shirota[M$Fed=="ufo"]) + P[4] * mean(M$Population[M$Fed=="ufo"]) + P[1] , b=P[2] + P[13], col=2)

points(x=M$Income[M$Fed=="szfo"], y=M$Price[M$Fed=="szfo"], col=3)
abline(a=P[3] * mean(M$Shirota[M$Fed=="szfo"]) + P[4] * mean(M$Population[M$Fed=="szfo"]) + P[1] + P[7], b=P[2]+ P[14], col=3)

points(x=M$Income[M$Fed=="urfo"], y=M$Price[M$Fed=="urfo"], col=4)
abline(a=P[3] * mean(M$Shirota[M$Fed=="urfo"]) + P[4] * mean(M$Population[M$Fed=="urfo"]) + P[1] + P[8], b=P[2] + P[15], col=4)

points(x=M$Income[M$Fed=="pfo"], y=M$Price[M$Fed=="pfo"], col=5)
abline(a=P[3] * mean(M$Shirota[M$Fed=="pfo"]) + P[4] * mean(M$Population[M$Fed=="pfo"]) + P[1] + P[9], b=P[2] + P[16], col=5)

points(x=M$Income[M$Fed=="skfo"], y=M$Price[M$Fed=="skfo"], col=6)
abline(a=P[3] * mean(M$Shirota[M$Fed=="skfo"]) + P[4] * mean(M$Population[M$Fed=="skfo"]) + P[1] + P[10], b=P[2] + P[17], col=6)

points(x=M$Income[M$Fed=="sfo"], y=M$Price[M$Fed=="sfo"], col=7)
abline(a=P[3] * mean(M$Shirota[M$Fed=="sfo"]) + P[4] * mean(M$Population[M$Fed=="sfo"]) + P[1] + P[11], b=P[2] + P[18], col=7)

points(x=M$Income[M$Fed=="dfo"], y=M$Price[M$Fed=="dfo"], col=8)
abline(a=P[3] * mean(M$Shirota[M$Fed=="dfo"]) + P[4] * mean(M$Population[M$Fed=="dfo"]) + P[1] + P[6], b=P[2], col=8)

legend(3000,60,legend = c("ЦФО","ЮФО","СЗФО","УФО","ПФО","СКФО","СФО","ДВФО"), col = c(1,2,3,4,5,6,7,8), lwd=1, pch = c(1, 1, 1,1,1,1,1,1))

(P[2]+ P[14]) * 100000 + P[3] * 61 + P[4] * 533 + P[1] + P[7]