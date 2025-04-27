N <- 10000
W_p <- 10^(-4)
m <- 10

mu_a <- c(32, 23, 40, 23, 46, 51)
mu_s <-c(165, 227, 246, 227, 253, 186)
g <- c(0.72 ,0.72, 0.72, 0.72, 0.72, 0.8)
d <- c(0.01, 0.02, 0.02, 0.09, 0.06)
r <- 0.01
l <- 0.02




layer_number <- function(x,y,z){
  if ((x^2+y^2+(l-z)^2) <= r^2) {
    return(6)
  }
  depth <- 0
  layer <- 0
  for(end_layer in d){
    if(z < depth) return(layer)
    depth = depth + end_layer
    layer = layer + 1
  }
  return(7)
}

generate_cos_theta <- function(g){
  x <- runif(1)
  ifelse(g==0, 2*x - 1, (1 + g^2 - ((1 - g^2)/(1 - g  + 2*g*x))^2)/(2*g))
}

generate_S <- function(layer){
  u <- runif(1)
  lambda <- mu_a[layer] + mu_s[layer]
  return((-1/lambda) * log(1-u))
}

generate_direction <- function(v, cos_theta){
  sin_theta <- sqrt(1 - cos_theta^2)
  phi <- runif(1, 0, 2 * pi)
  vx <- v[1]
  vy <- v[2]
  vz <- v[3]
  
    new_vx <- vx*cos_theta + (sin_theta * (vx*vz * cos(phi) - vy * sin(phi)))/sqrt(1-vz^2)
    new_vy <- vy*cos_theta + (sin_theta * (vy*vz * cos(phi) + vx * sin(phi)))/sqrt(1-vz^2)
    new_vz <- vz*cos_theta - (sin_theta * cos(phi)*sqrt(1-vz^2))
    
    if(abs(vz)==1)
    {
      new_vx <- sin_theta * cos(phi)
      new_vy <- sin_theta * sin(phi)
      new_vz <- sign(vz) * cos_theta
    }
  
    new_v <- c(new_vx, new_vy, new_vz)
    
    return(new_v)
}

reflect <- 0
finished <- 0
absorbed <- list()

for(i in 1:N){
  W <- 1
  position <- c(0, 0, 0)
  v <- c(0, 0, 1)
  layer = layer_number(position[1], position[2], position[3])
  s <- generate_S(layer)
  position <- position + s * v
  
  while(TRUE){
    layer = layer_number(position[1], position[2], position[3])
    
    if(layer==0){
      reflect = reflect + 1
      break
    }
    if(layer==7){
      finished = finished + 1
      break
    }

    g_n = g[layer]
    mu_a_n = mu_a[layer]
    mu_s_n = mu_s[layer]
    
    p_absorb = mu_a_n/(mu_a_n + mu_s_n)
    u <- runif(1)
    if(u < p_absorb){
      absorbed[[length(absorbed)+1]] = list(x=position[1], z=position[3],W = W)
      break
    }
    
    dW <- W * (mu_a_n/(mu_a_n + mu_s_n))
    W <- W - dW
    absorbed[[length(absorbed)+1]] = list(x=position[1], z=position[3],W = dW)
    if(W < W_p){
      u <- runif(1)
      if(u < 1/m){
        W <- W*m
      }else{
        absorbed[[length(absorbed)+1]] = list(x=position[1], z=position[3],W = W)
        break
      }
    }
    
    s <- generate_S(layer)
    cos_theta <- generate_cos_theta(g_n)
    v <- generate_direction(v, cos_theta)
    position <- position + s * v
  }
}

print("Поглощённые фотоны")
(N-finished-reflect) / N
print("Отражённые фотоны")
reflect / N
print("Прошедшие фотоны")
finished / N


create_circle <- function(center_x = 0, center_y = 0, radius = 1, npoints = 300) {
  angles <- seq(0, 2 * pi, length.out = npoints)
  data.frame(
    x = center_x + radius * cos(angles),
    y = center_y + radius * sin(angles)
  )
}

depth <- sum(d)
circle_df <- create_circle(center_x = 0, center_y = l, radius = r)

df <- do.call(rbind, lapply(absorbed, function(gr) {
  data.frame(x = gr$x, z = gr$z, W = gr$W)
}))


graf <- ggplot(df, aes(x = x, y = z, color = W)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Распределение поглощенной энергии в плоскости XZ",
       x = "Координата X",
       y = "Координата Z",
       color = "Энергия (W)") + 
  geom_hline(
    yintercept = d[1], 
    color = "black", 
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = d[2], 
    color = "black", 
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = d[3], 
    color = "black", 
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = d[4], 
    color = "black", 
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = d[5], 
    color = "black", 
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = d[6], 
    color = "black", 
    linetype = "dashed"
  ) +
  geom_path(
    data = circle_df, 
    aes(x = x, y = y),
    color = "black",
    linetype = "solid"
  ) +
  theme_minimal()

graf <- graf + coord_cartesian(ylim = c(0, depth))
graf


graf2 <- ggplot(df, aes(x = x, y = z)) +
  stat_summary_2d(
    aes(z = W),
    fun = function(x) log(sum(x)),  # или fun = sum
    bins = 50    # Количество ячеек по X и Z
  ) +
  scale_fill_gradientn(colors = c("blue", "red"))+
  geom_hline(
    yintercept = d[1], 
    color = "black", 
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = d[2], 
    color = "black", 
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = d[3], 
    color = "black", 
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = d[4], 
    color = "black", 
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = d[5], 
    color = "black", 
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = d[6], 
    color = "black", 
    linetype = "dashed"
  ) +
  geom_path(
    data = circle_df, 
    aes(x = x, y = y),
    color = "black",
    linetype = "solid"
  ) 
graf2 <- graf2 + coord_cartesian(ylim = c(0, depth))
graf2
