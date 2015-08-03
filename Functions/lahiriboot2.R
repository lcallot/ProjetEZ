
#sortie : 
# 1) size of confidence interval
# 2) coverage based on first ALASSO estimation
# 3) coverage based on true parameter
# 4) nb de faux nul 
# 5) mean 1st bias 
# 6) mean 2sd bias 

lahiriboot2<-function(data,iter,alpha,nonzero,post,betatrue){
  lass<-funboot(data,iter,post)
  las<-matrix(unlist(lass[1]),dim(data)[2]-1,iter)
  beta<-matrix(unlist(lass[2]))
  qnorm1<-qnorm(alpha/2,mean=0,sd=1)/sqrt(dim(data)[1])
  qnorm2<-qnorm(1-alpha/2,mean=0,sd=1)/sqrt(dim(data)[1])
  dist<-rep(0,nonzero)
  cover<-rep(0,nonzero)
  covertrue<-rep(0,nonzero)
  fn<-rep(0,nonzero)
  
  for (j in 1:nonzero){
    dist[j]<-quantile(las[j,], 1-alpha/2)-quantile(las[j,], alpha/2)
    g<-NULL
    gtrue<-NULL
    fauxnegatif<-NULL
    for (i in (1:iter)){
      g[i]<-sum((las[j,i]<=(qnorm2+beta[j]))&(las[j,i]>=(qnorm1+beta[j])))
      gtrue[i]<-sum((las[j,i]<=(qnorm2+betatrue[j]))&(las[j,i]>=(qnorm1+betatrue[j])))
      fauxnegatif[i]<-sum((las[j,i]==0))
    }
    cover[j]<-sum(g)/iter 
    covertrue[j]<-sum(gtrue)/iter 
    fn[j]<-sum(fauxnegatif)
    
  }  
  distnonzero<-mean(dist)
  covernonzero<-mean(cover)
  covernonzerotrue<-mean(covertrue)
  sumfn<-sum(fn)
  return(list(distnonzero,covernonzero,covernonzerotrue,sumfn))
}