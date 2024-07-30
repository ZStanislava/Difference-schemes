library(ggplot2)


psi = function(x) {
  return( 2*exp(x))
}

phi= function(x, t) {
  return(cos(t)/(1+x^4))
}

Ua = function(t) {
  return( 1 + exp(-t))
}

Ub = function(t) {
  return( 2*exp(2-4*t))
}

solve = function(h, tau) {
  a = 0
  b = 2
  p = 4
  T_ = 3
  x_ = seq(from = a, to = b, by = h)
  t_ = seq(from = 0, to = T_, by = tau)
  N = length(t_)
  M = length(x_)
  
  alpha = seq(along.with = x_)
  beta = seq(along.with = x_)
  
  r = p*tau/(h*h)
  an = -p*r
  bn = 1+2*p*r
  cn=an
  
  U = matrix(0, nrow = N, ncol = M)
  for(j in 1:M) {
    U[1, j] = psi(x_[j])
  }
  for(i in 1:N) {
    U[i, 1] = Ua(t_[i])
    U[i, M] = Ub(t_[i])
  }
  
  for(i in 2:N) {
    alpha[1] = 0
    for(j in 2:M) {
      alpha[j] = (-1)*an/(bn + cn*alpha[j-1])
    }
    
    beta[1] = Ua(t_[i])
    for(j in 2:M) {
      beta[j] = (phi(x_[j], t_[i-1])*tau + U[i-1, j] - cn*beta[j-1]) / 
        (bn + cn*alpha[j-1])
    }
    
    for(j in (M-1):2) {
      U[i, j] = alpha[j] * U[i, j+1] + beta[j]
    }
  }
  
  return(U)
}

func = function(h1, tau1, h2, tau2) {
  U1 = solve(h1, tau1)
  U2 = solve(h2, tau2)
  a = 0
  b = 2
  T_ = 3
  x1 = seq(from = a, to = b, by = h1)
  x2 = seq(from = a, to = b, by = h2)
  t1 = seq(from = 0, to = T_, by = tau1)
  t2 = seq(from = 0, to = T_, by = tau2)
  t = intersect(t1, t2)
  if (length(t) <= 4) {
    for (i in 1:length(t)) {
      df1 = data.frame(A = x1, B = U1[which(t1 == t[i]),]) 
      df2 = data.frame(C = x2, D = U2[which(t2 == t[i]),])
      g = ggplot() + geom_line(data = df1, aes(x = A, y = B), color = "pink2") +
        geom_line(data = df2, aes(x = C, y = D), color = "seagreen3") +
        labs(title = t[i])
      print(g)
    }
  }
  else {
    N = c(0, t[length(t) / 3], t[2 * length(t) / 3], T_)
    for (i in 1:4) {
      df1 = data.frame(A = x1, B = U1[which(t1 == N[i]),]) 
      df2 = data.frame(C = x2, D = U2[which(t2 == N[i]),])
      g = ggplot() + geom_line(data = df1, aes(x = A, y = B), color = "pink2") +
        geom_line(data = df2, aes(x = C, y = D), color = "seagreen3") +
        labs(title = N[i])
      print(g)
    }
  }
}

