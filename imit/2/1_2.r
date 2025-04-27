n <- 9
N <- 10000

gs <- c()
lens <- c()

for(k in 1:N){
  g <- 1
  map <- matrix(rep(FALSE, (2*n+1)^2),2*n+1, 2*n+1)
  point <- c(n+1, n+1)
  map[point[1], point[2]] = TRUE
  
  step <- 0
  while(step < n){
    possible_path <-list()
    for(i in -1:1){
      for(j in -1:1){
        if (abs(i+j)!=1) next
        possible_point <- c(point[1] + i, point[2] + j)
        if (map[ possible_point[1] , possible_point[2] ] == FALSE)
          possible_path[[length(possible_path) + 1]] = possible_point
      }
    }
    
    g = g * length(possible_path)
    if (length(possible_path)==0)break
    
    direction <- sample(length(possible_path),1)
    point = possible_path[[direction]]
    map[point[1], point[2]] = TRUE
    step = step + 1
  }
  if(g==0)next
  gs <- c(gs, g)
  lens <- c(lens, sqrt((point[1]-n-1)^2 + (point[2]-n-1)^2))
}
gs
map
result <- 0
v = gs / sum(gs)
v

for (i in 1:length(gs)) {
  result = result + v[i]*lens[i]
}

result






