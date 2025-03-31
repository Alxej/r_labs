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
in_triangle(c(1,1), c(0,0), c(0,1), c(1,0))

monte_carlo <- function(triangle1, triangle2, p){
  k <- 0
  min_x <- min(c(triangle1[,1], triangle2[,1]))
  max_x <- max(c(triangle1[,1], triangle2[,1]))
  min_y <- min(c(triangle1[,2], triangle2[,2]))
  max_y <- max(c(triangle1[,2], triangle2[,2]))
  i <- 0
  while(i<p)
  {
    x <- runif(1, min_x, max_x)
    y <- runif(1, min_y, max_y)
    if(in_triangle(c(x, y), triangle1) & in_triangle(c(x, y), triangle2))
    {
      k <- k+1
    }
    i <- i+1
  }
  print((max_x - min_x)*(max_y-min_y))
  print((k/p))
  s <- (max_x - min_x)*(max_y-min_y) * (k/p)
  return(s)
}
triangle1 <- matrix(c(0, 0, 7, 0 ,0, 7), ncol=2, byrow=TRUE)
triangle2 <- matrix(c(1, 1 ,6, 1, 1, 6), ncol=2, byrow=TRUE)

area = monte_carlo(triangle1, triangle2, 100000)
area