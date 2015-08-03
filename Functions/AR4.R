depAR4<-function(p,nonzero,alpha1,alpha2,alpha3,alpha4,t){
  beta=matrix(c(alpha1,alpha2,alpha3,alpha4,rep(0,p-nonzero)))
  z<-NULL
  z[1]<-rnorm(1,0,1)
  z[2]<-rnorm(1,0,1)
  z[3]<-rnorm(1,0,1)
  z[4]<-rnorm(1,0,1)
  for (i in 5:(t+p)){
    z[i]<-beta[1]*z[i-1]+beta[2]*z[i-2]+beta[3]*z[i-3]+beta[4]*z[i-4]+rnorm(1,0,1)
  }
  
  Z<-matrix(0,t,p+1)
  for (k in 1:(p+1)){
    Z[,k]<-matrix(z[(p+1-(k-1)):(t+p+1-k)])
  }
  y<-Z[,1]
  df2<-data.frame(cbind(y,Z[,-1]))
  return(df2)
}