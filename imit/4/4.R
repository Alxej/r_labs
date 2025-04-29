setwd("D:/r labs/imit/4")
library(MASS)

dt = 0.5
alpha = 0.6
sigma <- 0.5
N <- 1000
T <- 501
L <- 6
P0 <- 90
beta <- 3

U <- matrix(c(0, 0, 3.5, 0, 0, 3.5, -3.5, 0, 0, -3.5), ncol=5)
P <- (matrix(c(rep(1,25)), ncol=5) + diag(15, ncol=5, nrow=5)) / 20

Ao = matrix(c(1,0,0,dt,1,0,(dt^2)/2, dt, alpha), ncol=3)
Bo = matrix(c((dt^2)/2, dt, 0), ncol=1)
Co = matrix(c((dt^2)/2, dt, 1), ncol=1)

A <- rbind(cbind(Ao, matrix(c(rep(0,9)), ncol=3)), cbind(matrix(c(rep(0,9)), ncol=3), Ao))
B <- rbind(cbind(Bo, matrix(c(rep(0,3)), ncol=1)), cbind(matrix(c(rep(0,3)), ncol=1), Bo))
C <- rbind(cbind(Co, matrix(c(rep(0,3)), ncol=1)), cbind(matrix(c(rep(0,3)), ncol=1), Co))

stations <- read.table("stations.txt")
rssi <- read.table("RSSI-measurements.txt")

x <- mvrnorm(N, c(rep(0,6)), diag(c(500,5,5,200,5,5))) # начальное состояние робота
x_moved <- matrix(rep(0, N*6), ncol=6) 
direction <- sample(5,N, replace=TRUE) 

w <- c(rep(1/N,N))
path_x <- c(rep(0,T)) 
path_y <- c(rep(0,T))


generate_w <- function(position, t){
  delta <- 1.5
  z_teor <- c(rep(0,L)) # посчитанные сигналы
  z <- c(rep(0,L)) # наблюдаемые сигналы
  for (i in 1:L){
    z[i] <- rssi[i,t]
    distance <- sqrt(sum((position - stations[,i])^2))
    z_teor[i] = P0 - 10*beta * log(distance, base=10)
  }
  exp((-1/(2*delta^2))*sum((z - z_teor)^2))
}


for (t in 1:T){
  for(i in 1:N){
    u <- U[,direction[i]]
    x_moved[i,] <- t(A%*%x[i,] + B%*%u + C%*%mvrnorm(1, c(0,0), diag(sigma^2, ncol=2, nrow=2)))
    position <- c(x_moved[i,1], x_moved[i,4])
    w[i] <- generate_w(position, t)
  }
  w <- w / sum(w) # нормируем веса
  
  new_direction <- c(rep(0,N))
  for(i in 1:N){
    x_id <- sample(N, 1, prob=w) # выбираем частицу из старых
    x[i,] <- x_moved[x_id,]
    new_direction[i] <- sample(5,1,prob=P[direction[x_id],]) # выбираем направление используя матрицу перехода
  }
  path_x[t] <- sum(x[,1])/N # считаем координаты робота, как среднее частиц с одинаковым весом
  path_y[t] <- sum(x[,4])/N
  
  direction <- new_direction
}

plot(c(0, 0, 3464.102, 3464.102, -3464.102, -3464.102), c(4000, -4000, 2000, -2000, -2000, 2000), xlab='x', ylab='y')
lines(path_x, path_y, type='l')
