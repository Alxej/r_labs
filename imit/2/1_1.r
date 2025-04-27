n <- 10
m <- 30
k <- 6
lambda <- c(rep(4, n), rep(2,m))
lambda
N <- 1000
sum <- 0


F = function(j, x) {
  1 - exp(-lambda[j]*x)
}

for (q in 1:N) {
  T = rep(0, m)
  for (i in 1:m) {
    u <- runif(1, min = 0, max = 1)
    T[i] <- (-1/lambda[n+i]) * log(1-u)
  }
  T <- sort(T, TRUE)
  T_k <- T[k]
 
  pr <- 1
  for (j in 1:n) {
    pr <- pr*F(j,T_k)
  }
  sum = sum + pr
}
T
sum/N
