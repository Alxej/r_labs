setwd("D:/r labs/data/")
T<-read.table("reg.txt", header=TRUE)

second_var = "A12"
plot(x=T$A1, y=T[[second_var]])

cor(T$A1, T[[second_var]])
cor.test(T$A1, T[[second_var]])

T3<-T[-which(T$A1>1000 & T[[second_var]]<600),c(1,12)]
T3
plot(x=T3$A1, y=T3[[second_var]])
cor(T3$A1, T3[[second_var]])
cor.test(T3$A1, T3[[second_var]])
