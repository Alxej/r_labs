setwd("D:/r labs/imit/3") # установить рабочую папку
test <- 2

if (test == 1) {
  C <- read.table("test1.txt")
  T <- 3
  a <- 1
  b <- 1
} else {
  C <- read.table("test2.txt")
  T <- 20
  a <- 0.1
  b <- 0.1
}

D <- max(C$V1)
W <- max(C$V2)

NWT <- matrix(0, nrow=W, ncol=T)
NTD <- matrix(0, nrow=T, ncol=D)
NT <- c(rep(0, T))

Nd <- c(rep(0, D))

Z <- list()

for (stroka in 1:length(C$V1)){
  zs <- c()
  for (i in 1:C$V3[stroka]) {
    z <- sample(T, 1)
    zs <- c(zs, z)
    w <-  C$V2[stroka]
    d <- C$V1[stroka]
    NWT[w, z] <- NWT[w, z] + 1
    NTD[z,d] <- NTD[z,d] + 1
    NT[z] <- NT[z] + 1
    Nd[d] <- Nd[d] + 1
  }
  Z[[stroka]] <- zs
}

M <- 10

theta <- matrix(0, nrow=D, ncol=T)
phi <- matrix(0, nrow=T, ncol=W)

discr = function(P) {
  u <- runif(1, min = 0, max = 1)
  i <- 1
  p <- P[1]
  while (u > p) {
    i <- i + 1
    p <- p + P[i]
  }
  return(i)
}

for (m in 1:M) {
  for (stroka in 1:length(C$V1)){
    for (i in 1:C$V3[stroka]) {
      z <- Z[[stroka]][i]
      w <-  C$V2[stroka]
      d <- C$V1[stroka]
      NWT[w,z] <- NWT[w,z] - 1
      NTD[z,d] <- NTD[z,d] - 1
      NT[z] <- NT[z] - 1
      P <- c(rep(0, T))
      for (t in 1:T) {
        P[t] = (a + NTD[t,d])*(b + NWT[w,t])/(b*W+NT[t])
      }
      P <- P / sum(P)
      
      z <- discr(P)
      
      NWT[w, z] <- NWT[w, z] + 1
      NTD[z,d] <- NTD[z,d] + 1
      NT[z] <- NT[z] + 1
      Z[[stroka]][i] <- z
    }
  }
  
  for (t in 1:T) {
    for (d in 1:D) {
      theta[d,t] <- theta[d,t] + (a + NTD[t,d]) / (a*T + Nd[d])
    }
    
    for (w in 1:W) {
      phi[t,w] <- phi[t,w] + (b + NWT[w,t]) / (b*W + NT[t])
    }
  }
}

theta / M
phi / M