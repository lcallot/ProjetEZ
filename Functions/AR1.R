dep<-function(alpha,t,p,nonzero){
  beta=matrix(c(alpha,rep(0,p-nonzero)))
  z<-NULL
  z[1]<-rnorm(1,0,1)
  for (i in 2:(t+p)){
    z[i]<-beta[1]*z[i-1]+rnorm(1,0,1)
  }
  
  Z<-matrix(0,t,p+1)
  for (k in 1:(p+1)){
    Z[,k]<-matrix(z[(p+1-(k-1)):(t+p+1-k)])
  }
  return(Z)
}