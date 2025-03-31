f <- function(x) {
  ifelse(x < 0 | x > 2 * pi, 0, (1/(4 * pi))*(2 + cos(x)))
}
F <- function(x) {
  ifelse(x<0, 0, ifelse(x > 2*pi, 1, (sin(x) + 2*x)/(4*pi)))
}

i <- 0
n <- 10000
v<- numeric(n)

a <- -1
b <- 2* pi

# Метод Неймана
M = optimize(f, lower= a, upper= b, maximum=TRUE)$objective # Максимум функции на заданом отрезке

while(i<n){
  u1 <- runif(1, 0, 1)
  u2 <- runif(1, 0, 1)
  
  x1 <- a + (b - a) * u1
  x2 <- M * u2
  
  if(x2<=f(x1)){
    v[i] <- x1
    i <- i+1
  }
}
hist(v,breaks=45, freq = FALSE)
x <- seq(0, 2*pi, length.out=1000)
lines(x, f(x), col = "red")

r = ks.test(v, F)
r 
if (0.05 <= r$p.value){
  print("Нет оснований отвергать нулевую гипотезу.")
  } else {
  print("Отвергаем нулевую гипотезу")}
