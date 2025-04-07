library(plotly)

u_min <- 0
u_max <- 2 * pi

v_min <- 0
v_max <- 2 * pi

#M <- sqrt((2 * pi)^2 + 1)


p <- function(u) {
  integral_value <- integrate(function(u) sqrt(u^2 + 1), u_min, u_max)$value
  return(sqrt(u^2 + 1) / integral_value)
}
M <- optimize(p, lower= 0, upper= 2*pi, maximum=TRUE)$objective
M

generate_u <- function(n) {
  i <- 1
  u <- numeric(n)
  while(i<n){
    u1 <- runif(1, 0, 1)
    u2 <- runif(1, 0, 1)
    
    x1 <- 2 * pi * u1
    x2 <- M * u2
    
    if(x2<=p(x1)){
      u[i] <- x1
      i <- i+1
    }
  }
  return(u)
}


points <- 10000
u <- generate_u(points)
v <- runif(points, v_min, v_max)

x <- u * cos(v)
y <- u * sin(v)
z <- v

data <- data.frame(x = x, y = y, z = z)


plot_ly(data, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'markers', marker = list(size = 2, color = z, colorscale = 'Viridis', showscale = TRUE)) %>%
  layout(scene = list(xaxis = list(title = 'X'),
                      yaxis = list(title = 'Y'),
                      zaxis = list(title = 'Z')),
         title = "Геликоид")