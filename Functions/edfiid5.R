edfiid5<-function(p,beta1,beta2,beta3,beta4,beta5,n,iter,alpha){
  beta<-matrix(c(beta1,beta2,beta3,beta4,beta5,rep(0,p-5)))
  size<-rep(0,iter)
  cov<-rep(0,iter)
  for (j in 1:iter){
    norm<- matrix(rnorm(n*p,0,1),n,p)
    y<-norm%*%beta+matrix(rnorm(n,0,1),n,1)
    df<-data.frame(y,norm)
    size[j]<-lahiri(df,alpha)[1]
    cov[j]<-lahiri(df,alpha)[2]
  }
  v1<-mean(size)
  v2<-mean(cov)
  return(c(v1,v2))
}


