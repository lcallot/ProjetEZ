depAR5<-function(p,gamma1,gamma2,gamma3,gamma4,gamma5,t){
  z<-NULL
  z[1]<-rnorm(1,0,1)
  z[2]<-rnorm(1,0,1)
  z[3]<-rnorm(1,0,1)
  z[4]<-rnorm(1,0,1)
  z[5]<-rnorm(1,0,1)
  for (i in 6:(t+p)){
    z[i]<-gamma1*z[i-1]+gamma2*z[i-2]+gamma3*z[i-3]+gamma4*z[i-4]+gamma5*z[i-5]+rnorm(1,0,1)
  }
  
  Z<-matrix(0,t,p+1)
  for (k in 1:(p+1)){
    Z[,k]<-matrix(z[(p+1-(k-1)):(t+p+1-k)])
  }
  y<-Z[,1]
  df<-data.frame(cbind(y,Z[,-1]))
  return(df)
}