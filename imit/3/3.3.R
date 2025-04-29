library(plot3D)

get_b = function(i, j) {
  sum1 <- 0
  sum2 <- 0
  
  if (i-1 > 0) sum1 <- sum1 + x[i-1,j]
  if (i+1 <= n) sum1 <- sum1 + x[i+1,j]
  if (j-1 > 0) sum2 <- sum2 +  x[i,j-1]
  if (j+1 <= n) sum2 <- sum2 + x[i,j+1] 
  
  b1*sum1 + b2*sum2
}

get_a = function(i,j) {
  count1_eq_x <- 0
  count1_not_eq_x <- 0
  count2_eq_x <- 0
  count2_not_eq_x <- 0
  
  if (i-1 > 0) {
    if (x[i-1,j] == x[i,j]) count1_eq_x <- count1_eq_x + 1
    else count1_not_eq_x <- count1_not_eq_x + 1
  }
  if (i+1 <= n) {
    if (x[i+1,j] == x[i,j]) count1_eq_x <- count1_eq_x + 1
    else count1_not_eq_x <- count1_not_eq_x + 1
  }
  if (j-1 > 0) {
    if (x[i,j-1] == x[i,j]) count2_eq_x <- count2_eq_x + 1
    else count2_not_eq_x <- count2_not_eq_x + 1
  }
  if (j+1 <= n) {
    if (x[i,j+1] == x[i,j]) count2_eq_x <- count2_eq_x + 1
    else count2_not_eq_x <- count2_not_eq_x + 1
  }
  
  exp(-2*b1*(count1_eq_x - count1_not_eq_x))*exp(-2*b2*(count2_eq_x - count2_not_eq_x))
}


n <- 100
b1 <- 30
b2 <- 20

m <- 100
x <- matrix(2*(sample(2, n*n, replace = TRUE)-1)-1, n, n)
for (k in 1:m) {
  for (i in 1:n) {
    for (j in 1:n) {
      b <- get_b(i,j)
      p <- 1/(1+exp(-2*b))
      u <- runif(1, min = 0, max = 1)
      x[i,j] = 2*(u < p) - 1
    }
  }
}

image2D(x)


m <- 1000000
x <- matrix(2*(sample(2, n*n, replace = TRUE)-1)-1, n, n)
for (k in 1:m) {
  i <- sample(n, 1)
  j <- sample(n, 1)
  a <- min(1, get_a(i,j))
  u <- runif(1, min = 0, max = 1)
  x[i,j] = 2*(u < p) - 1
}
image2D(x)
