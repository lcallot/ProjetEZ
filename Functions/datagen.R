#iid1

iid1<-function(p,beta,n){
  norm<-NULL
  for (i in 1:p){
    norm<-cbind(norm,as.matrix(rnorm(n,0,1),n,1))
  }
  bet<-matrix(c(beta,rep(0,p-1)))
  y<-norm%*%bet+matrix(rnorm(n,0,1),n,1)
  df<-data.frame(y,norm)
  return(df)
}
iid1(10,3,100)

#iid5

iid5<-function(p,beta1,beta2,beta3,beta4,beta5,n){
  norm<-NULL
  for (i in 1:p){
    norm<-cbind(norm,as.matrix(rnorm(n,0,1),n,1))
  }
  beta<-matrix(c(beta1,beta2,beta3,beta4,beta5,rep(0,p-5)))
  y<-norm%*%beta+matrix(rnorm(n,0,1),n,1)
  df<-data.frame(y,norm)
  return(df)
}

#iid10

iid10<-function(p,beta1,beta2,beta3,beta4,beta5,beta6,beta7,beta8,beta9,beta10,n){
  norm<-NULL
  for (i in 1:p){
    norm<-cbind(norm,as.matrix(rnorm(n,0,1),n,1))
  }
  beta<-matrix(c(beta1,beta2,beta3,beta4,beta5,beta6,beta7,beta8,beta9,beta10,rep(0,p-10)))
  y<-norm%*%beta+matrix(rnorm(n,0,1),n,1)
  df<-data.frame(y,norm)
  return(df)
}



# AR 1

depAR1<-function(p,gamma,t){
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

# AR5

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