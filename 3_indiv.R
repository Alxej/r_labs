library(forecast)

setwd("C:/xd/r_labs/data")
M<-read.csv2 ("Series_G_1.csv", header=TRUE, sep=";", dec = ",")
M <- head(M,60)

plot(M$ip, main="Индустриальное производство", ylab="млн. $", xlab="месяц", type="o", col="blue")
l <- acf(M$ip, type="correlation", plot=TRUE, main="Коррелограмма")
# судя по всему 12 - период колебаний, хотя не до конца понятно по рисунку, однако будет видно при следующей работе

sn <- ma(M$ip, order=12, centre = TRUE)
M$ip
sn
plot(M$ip, main="Индустриальное производство", ylab="млн. $", xlab="месяц", type="o")
lines(sn, col="green") # нарисовать сглаженный ряд

A<-matrix(data=M$ip-sn, nrow = 12) 
A
SM<-apply(A, 1, function(x) mean(x, na.rm = TRUE))
SM
SM <- SM - sum(SM, na.rm = TRUE) / 12
SM
M.S<-rep(SM,times=5)
M.S

Tr<-M$ip-M.S
T<-seq(from=1, to=60)
regM<-lm(Tr~T)
M.Trend<-coef(regM)[1]+coef(regM)[2]*T 

M.fit<-M.Trend+M.S # рассчитать значения временного ряда по модели
plot(M$ip, main="Индустриальное производство", ylab="млн. $", xlab="месяц", type="o") # график временного ряда

lines(M.fit, col="red") # график модели временного ряда
lines(M.Trend, col="green") # график тренда временного ряда
sum(abs((M$ip - M.fit)/M$ip))/length(M$ip)*100 # рассчитать MAPE

# 5.22 - ошибка аппроксимации

M.F<-array(dim = 36) # создать массив для хранения прогноза
T1<-seq(from=61, to=60+36) # создать массив для времени прогноза
M.F<-(coef(regM)[1]+coef(regM)[2]*T1)+SM # рассчитать прогнозные значения
plot(M$ip, main="Индустриальное производство", ylab="млн. $", xlab="месяц", type="o", xlim =
       c(1,60+36), ylim = c(min(M$ip), 200)) # график временного ряда
lines(M.fit, col="red") # график модели временного ряда
lines(x=T1 , y=M.F, col="green") # график прогноза
abline(regM)

M.Res<-M$ip-M.fit # рассчитать остатки
plot(M.Res, type="o", main="Остатки", ylab="млн. $.", xlab="месяц") # график остатков
acf(M.Res, main="Коррелограмма остатков") # коррелограмма остатков
Box.test(M.Res) # проверить остатки на белый шум
