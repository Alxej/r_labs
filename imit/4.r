k <- 10000
D <- 5
lambda <- 2
td <- 3 * 10^(-8)
c <- 3 * 10^(8)
ok <- 0



for (a in 1:k) {
  n <- rpois(1, lambda) + 1 # кол-во фотонов
  
  t <- c()
  
  for (i in 1:n){
    y0 <- runif(1, 0, D)
    alp <- runif(1, 0, 2*pi)
    if (alp > 0 & alp < pi){
      s <- (D -y0) / sin(alp)
      time <- s/c
      t <- c(t, time)
    } else if(alp < 2*pi & alp > pi){
      s <- y0 / -(sin(alp))
      time <- s/c
      t <- c(t, time)
    }
  }
  t <- sort(t)
  l <- length(t)
  if (l < 2) next
  for(i in 1:(l-1))
  {
      delta <- t[i + 1] - t[i]
      if (delta < td) {
        ok = ok + 1
        break
      }
  }
}
p <- ok/k
p
