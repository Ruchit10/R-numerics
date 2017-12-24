# Finite Volume and different Schemes
M = 100
N = 400
dx = 10/M
i <- c(1:(M-1))
x <- i*dx
U <- array(0,dim = c(M-1,N-1))
v <- 10 + x
nu = 0.8
dt <- (nu*dx) / max(v)
nu2 <- (v*dt) / dx
un_j <- c()
for (j in 1:length(U[,1]))
{
  x0 = x[j]
  u0 = U[,1][j]
  if (x0>=1 & x0 <=2)
  {
    U[,1][j] <- 1
  }
  else
  {
  }
}
for (n in 1:(length(U[1,])-1))
{
  for(k in 2:length(U[,n]))
  {
    ujn = U[,n][k]
    uj_n = U[,n][(k-1)]
    ujnn = U[,n][k+1]
    un_j[k] <- 0.5*(ujnn + uj_n) - 0.5*nu2[k]*(ujnn - uj_n)
    for (m in 1:length(un_j))
    {
      ch <- is.na(un_j[m])
      if (ch==TRUE)
      {
        un_j[m] <- 0.0
      }
      else
      {
        
      }
    }
  }
  U[,(n+1)] <- un_j
  print(n)
}
#saveHTML(for(i in 1:59)plot(x,U[,i],col=2,type='l',lwd=2))