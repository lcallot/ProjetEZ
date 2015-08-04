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