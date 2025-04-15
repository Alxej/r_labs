znak <- function(p, p1, p2){
  (p1[1] - p[1])*(p2[2] - p1[2]) - (p2[1]-p1[1])*(p1[2]-p[2])
}

in_triangle <- function(p, triangle){
  t1 <- triangle[1,]
  t2 <- triangle[2,]
  t3 <- triangle[3,]
  z12 <- znak(p, t1, t2)
  z23 <- znak(p, t2, t3)
  z31 <- znak(p, t3, t1)
  
  ifelse((z12>=0 & z23>=0 & z31>=0)|(z12<=0 & z23<=0 & z31<=0), TRUE, FALSE)
}

monte_carlo <- function(triangle1, triangle2, p, plot_points = FALSE){
  k <- 0
  min_x <- min(c(triangle1[,1], triangle2[,1]))
  max_x <- max(c(triangle1[,1], triangle2[,1]))
  min_y <- min(c(triangle1[,2], triangle2[,2]))
  max_y <- max(c(triangle1[,2], triangle2[,2]))
  
  # Подготовка данных для визуализации
  if(plot_points) {
    plot_x <- numeric(p)
    plot_y <- numeric(p)
    plot_col <- character(p)
  }
  
  i <- 0
  while(i < p) {
    x <- runif(1, min_x, max_x)
    y <- runif(1, min_y, max_y)
    
    in_t1 <- in_triangle(c(x, y), triangle1)
    in_t2 <- in_triangle(c(x, y), triangle2)
    
    if(plot_points) {
      plot_x[i+1] <- x
      plot_y[i+1] <- y
      plot_col[i+1] <- ifelse(in_t1 & in_t2, "red", "blue")
    }
    
    if(in_t1 & in_t2) {
      k <- k + 1
    }
    i <- i + 1
  }
  
  rect_area <- (max_x - min_x)*(max_y - min_y)
  prob <- k / p
  s <- rect_area * prob
  
  if(plot_points) {

    plot(NA, xlim = c(min_x, max_x), ylim = c(min_y, max_y), 
         xlab = "X", ylab = "Y", 
         main = paste("Метод Монте-Карло\nПлощадь пересечения:", round(s, 4)))
    

    polygon(triangle1, col = rgb(0, 0, 1, 0.1), border = "blue", lwd = 2)
    polygon(triangle2, col = rgb(0, 1, 0, 0.1), border = "green", lwd = 2)
    

    points(plot_x, plot_y, col = plot_col, pch = 20, cex = 0.5)
    

    legend("topright", 
           legend = c("Треугольник 1", "Треугольник 2", "В пересечении", "Не в пересечении"),
           fill = c(rgb(0, 0, 1, 0.3), rgb(0, 1, 0, 0.3), "red", "blue"),
           border = c("blue", "green", NA, NA))
  }
  
  return(s)
}

triangle1 <- matrix(c(0, 0, 7, 0, 0, 7), ncol = 2, byrow = TRUE)
triangle2 <- matrix(c(1, 1, 6, 1, 1, 6), ncol = 2, byrow = TRUE)

area <- monte_carlo(triangle1, triangle2, 10000, plot_points = TRUE)

