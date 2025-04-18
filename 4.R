setwd("C:/xd/r_labs/data/")
T<-read.table("reg.txt", header=TRUE)
second_var <- "A7"
T<-T[order(T$A1),]
reg9<-lm(formula=T[[second_var]] ~T$A1)

reg9.1<-lm(formula=T[[second_var]] ~log(T$A1)) 
summary(reg9.1)
plot(x=T$A1,y=T[[second_var]]) # нарисовать диаграмму рассеяния
abline(reg9, col="red") # нарисовать линейную регрессию
coef(reg9.1)[1]
coef(reg9.1)[2]
curve(coef(reg9.1)[1] + coef(reg9.1)[2]*log(x), add=TRUE, col="green")
plot(reg9.1$residuals, main="График остатков", xlab="Номер наблюдения", ylab="Остатки")
abline(h=0, col="red")
library("forecast") 
accuracy(reg9.1)
coef(reg9.1)[1] + coef(reg9.1)[2]*log(1350)


nlc <- nls.control(maxiter = 200)
reg9.2<-nls(T[[second_var]]~p2 *(p1 + T$A1) * (p1 + T$A1) + p3, control = nlc, data=T, start=list(p1=-500, p2=-500, p3=100))
summary(reg9.2)
P<-coef(reg9.2)
plot(x=T$A1,y=T[[second_var]])
abline(reg9, col="red")
curve(coef(reg9.1)[1] + coef(reg9.1)[2]*log(x), add=TRUE, col="green")
curve(P[2] * (P[1] + x) * (P[1] + x)+ P[3], add=TRUE, col="blue") 
plot(reg9.2$m$resid(), main="График остатков", xlab="Номер наблюдения", ylab="Остатки")
abline(h=0, col="red")
sum(abs((T[[second_var]] - (P[2] * (P[1] + T$A1) * (P[1] + T$A1)+ P[3]))/T[[second_var]]))/length(T[[second_var]])*100
P[2] * (P[1] + 1350) * (P[1] + 1350)+ P[3]
P

