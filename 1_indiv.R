

setwd("C:/xd/r_labs/data/")

M<-read.csv2("sugar_prices.csv", header=TRUE, sep=";" , dec = ",")

M<-M[order(M$PriceRK),]

cor(M$PriceRK, M$Income)
cor.test(M$PriceRK, M$Income)
# Между средней ценой сахарного песка и средней начисленной зарплатой
# существует сильная линейная зависимость,
# значимая на уровне значимости 0,1

# Линейная модель
reg1<-lm(formula=M$PriceRK ~M$Income)
summary(reg1)
# уравнение y = 14.772 + 0.000925 * x + e описывает
# значимую на уровне значимости 0,1
# линейную зависимость
# между средней ценой сахарного песка и средней начисленной зарплатой
format(coef(reg1), digits = 6)
plot(x=M$Income, y=M$PriceRK)
abline(reg1, col="red")
# Регрессия значима, коэффициенты значимы на уровне значимости 0.1
mean(reg1$residuals)
plot(reg1$residuals, main="График остатков", xlab="Номер наблюдения", ylab="Остатки")
abline(h=0)

# График остатков распределен не равномерно, независимо
library("forecast") 
accuracy(reg1) # MAPE = 10.80, что немного 

# Нелинейная модель (log)
reg2<-lm(formula=M$PriceRK ~log(M$Income))
plot(x=M$Income, y=M$PriceRK)
abline(reg1, col="red")
curve(coef(reg2)[1] + coef(reg2)[2]*log(x), add=TRUE, col="green")
summary(reg2)
# Регрессия значима, коэффициенты значимы на уровне значимости 0.1

# График остатков распределен не равномерно, но лучше чем у линейной, независимо
plot(reg2$residuals, main="График остатков", xlab="Номер наблюдения", ylab="Остатки")
abline(h=0, col="red")
accuracy(reg2) # MAPE = 11.87, что немного, но больше чем у линейной


# Нелинейная модель (квадратичная)
nlc <- nls.control(maxiter = 200)
reg3<-nls(M$PriceRK~p2 *(p1 + M$Income) * (p1 + M$Income) + p3, control = nlc, data=M, start=list(p1=-500, p2=-500, p3=100))
summary(reg3) # Коэффициенты и регрессия незначимы
P<-coef(reg3)
plot(x=M$Income,y=M$PriceRK)
abline(reg1, col="red")
curve(coef(reg2)[1] + coef(reg2)[2]*log(x), add=TRUE, col="green")
curve(P[2] * (P[1] + x) * (P[1] + x)+ P[3], add=TRUE, col="blue")

# График остатков как у линейной
plot(reg3$m$resid(), main="График остатков", xlab="Номер наблюдения", ylab="Остатки")
abline(h=0, col="red")
sum(abs((M$PriceRK - (P[2] * (P[1] + M$Income) * (P[1] + M$Income)+ P[3]))/M$PriceRK))/length(M$PriceRK)*100
# MAPE = 10.73, почти как у линейной

# Выбираем линейную регрессию, так как у нее MAPE наименьший и все коэффициенты значимы
M<-M[1,]
M$Income<- 36000
M
format(predict(reg1, newdata=M, interval="confidence", level=0.9), digits=10)
# Вывод: если средняя начисленная зарплата будет равна 350000 руб,
# то средняя цена сахарного песка будет равнятся 47.155 рублей.
# С увеличением средней начисленной зарплаты на 1 т.р
# цена на сахарный песок в среднем увеличится на 0.92 рубля.

# Вывод: если средняя начисленная зарплата будет равна 350000 руб,
# то средняя цена сахарного песка будет равнятся от 45.87 рублей до 50.29 рублей
# при уровне значимости 0,1.
