setwd("C:/xd/r_labs/data/") # установить рабочую папку
M<-read.csv2("Milk09-12.csv", header=FALSE, sep=";", dec = ",", col.names =
               c("Month","Price1","Price2"))
plot(M$Price1, main="Цена на молоко", ylab="цена, руб.", xlab="месяц, номер", type="o")
x<-seq(from=1, to=length(M$Price1))
reg1<-lm(M$Price1~x)
abline(reg1)
plot(M$Price2, main="Цена на молоко", ylab="цена, руб.", xlab="месяц, номер", type="o")
reg2<-lm(M$Price2~x)
abline(reg2)

reg4<-nls(M$Price2~p1 * exp(p2*x), data=M, start=list(p1=1, p2=0), algorithm = "port") # построить нелинейную регрессию с параметрами
summary(reg4) # вывести коэффициенты регрессии
P<-coef(reg4) # сохранить коэффициенты регрессии в переменной Р
curve((P[2] * (P[1] + x) * (P[1] + x)+ P[3]), add=TRUE, col="blue") # нарисовать нелинейную регрессию

