
exact = function(x, t)
  return(0.5*t^2+2*sin(2+x)+2*exp(x-0.5*t)-2*sin(2+x-0.5*t))

drawExact = function(){
  x = seq(-0.2, 1.8, length.out = 100)
  t = seq(0, 2, length.out = 100)
  require(tidyr)
  points = crossing(t, x)
  u = exact(points$x, points$t)
  require(rgl)
  plot3d(points$x, points$t, u, col = 'orange')
}

psi = function(x)
  return(2*exp(x))

fi = function(x, t)
  return(t+cos(2+x))

schema = function(){
  h = 0.01
  q = -0.5 # константа от -1 до 0 
  
  r = q/(-0.5)
  
  tau = r * h
  
  a = -0.2
  b = 1.8
  
  n = abs(2 %/% tau) #  кол-во строк
  m = (b + n * h - a) %/% h + 1 #  кол-во столбцов
  
  layers = matrix(0, nrow = n + 1, ncol = m)
  
  
  for (j in 1:m)
    layers[1, j] = psi(a - n * h + (j-2) * h)

  
  k = 2
  for (i in 1:n){ #не все выводятся из тех,что задавали 
    for(j in k:m){
      layers[i + 1, j] = (1 + q) * layers[i, j] - q * layers[i, j - 1] + tau * fi(a - h*n + (j - 2) * h, tau * (i-1))
    }
    k = k + 1
  }
  
  x = seq(a, by = h, length.out = m - k + 1)
  t = seq(0, by = tau, length.out = n + 1)
  require(tidyr)
  points = crossing(t, x)
  
  todraw = numeric()
  for (i in 1:(n+1))
    todraw <- append(todraw, layers[i, k:m])

  require(rgl)
  points3d(points$x, points$t, todraw, col = 'blue', lwd = 4)
}

drawExact()
schema()