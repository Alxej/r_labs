m <- 10
n <- sample(1:20, m, replace=TRUE)
k <- 1000
T <- 1
a <- 0.05

s0 <- runif(m)
mu <- runif(m)
sigma <- runif(m)
z <- matrix(rnorm(k * m), nrow = k, ncol = m)

s_t <- s0 * exp((mu - sigma^2 / 2) * T + sigma * sqrt(T) * z)


v_t <- rowSums(s_t * matrix(n, nrow = k, ncol = m, byrow = TRUE))

l_t <- sum(n * s0) - v_t

VaR <- quantile(l_t, 1 - a)
VaR
