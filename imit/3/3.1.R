setwd("D:/r labs/imit/3") # установить рабочую папку
C <- c(read.table("data.txt")[1])
V <- 150
g_s <- 5
c_oa <- 400
sigma <- 5
y0 <- 5
t0 <- 0

t = c(1:length(C$V1))

N = 10000

find_x = function(lambda, t) {
  (lambda*exp(lambda*t0/V)*y0+(-c_oa*lambda-g_s)*exp(lambda*t0/V)) / (lambda*exp(lambda*t/V)) + g_s/lambda + c_oa
}

pi = function(lambda) {
  x <- find_x(lambda, t)
  y <- C$V1 #[1:1000]
  
  (-1/(2*sigma^2))*sum((y-x)^2) 
}

lambda <- 1
lambds <- c()
argmax <- 1
pi_max <- -10^6

for (i in 1:N) {
  u <- runif(1, min = 0, max = 1)
  candidat_lambda <- rnorm(1, lambda, 0.1)
  pi_cl <- pi(candidat_lambda)
  pi_l <- pi(lambda)
  a <- min(pi_cl-pi_l, 1)
  if (log(u) <= a) {
    lambda <- candidat_lambda
    lambds <- c(lambds, lambda)
    if (pi_max < pi_cl) {
      pi_max <- pi_cl
      argmax <- lambda
    }
  } 
}

lambda
mean(lambds)
argmax


