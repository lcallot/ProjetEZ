edfAR4<-function(p,nonzero,gamma1,gamma2,gamma3,gamma4,n,iter,alpha){
  size<-rep(0,iter)
  cov<-rep(0,iter)
  for (j in 1:iter){
    Z<-depAR4(p,nonzero,gamma1,gamma2,gamma3,gamma4,n,iter)
    size[j]<-lahiri(Z,alpha)[1]
    cov[j]<-lahiri(Z,alpha)[2]
  }
  v1<-mean(size)
  v2<-mean(cov)
  return(c(v1,v2))
}