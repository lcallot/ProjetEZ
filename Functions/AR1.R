depAR1<-function(p,nonzero,gamma,t){
  gamma
  z<-NULL
  z[1]<-rnorm(1,0,1)
  for (i in 2:(t+p)){
    z[i]<-gamma*z[i-1]+rnorm(1,0,1)
  }
  
  Z<-matrix(0,t,p+1)
  for (k in 1:(p+1)){
    Z[,k]<-matrix(z[(p+1-(k-1)):(t+p+1-k)])
  }
  y<-Z[,1]
  df<-data.frame(cbind(y,Z[,-1]))
  return(df)
}