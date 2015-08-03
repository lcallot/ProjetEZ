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
