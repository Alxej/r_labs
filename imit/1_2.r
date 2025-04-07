library(MASS)
n <- 3
k <- 1000
mu <- c(2.5, 3.0, 4.2)
sig <- matrix(c(1.0, 0.5, 0.2,   0.5, 1.0, 0.4,  0.2, 0.4, 1.0 ), ncol=3, byrow=TRUE)

C <- t(chol(sig))
C

Y = t(mvrnorm(k, c(0, 0, 0), diag(n)))
Y
Z <- C %*% Y + mu

cov_mat = cov(t(Z))
cov_mat

i = 1
j = 2
cor = cov_mat[i, j] / sqrt(cov_mat[i,i] * cov_mat[j,j])
cor_teor = sig[i, j] / sqrt(sig[i,i] * sig[j,j])
cor
cor_teor


t = (0.5 * log((1 + cor) / (1 - cor)) - 0.5 * log((1 + cor_teor) / (1 - cor_teor))) / (1 / sqrt(k - 3))
t
qnorm(0.95)
if (abs(t) < qnorm(0.95)) {
  cat("Нет оснований отвергать нулевую гипотезу.")
} else {
  cat("Отвергаем нулевую гипотезу")
}