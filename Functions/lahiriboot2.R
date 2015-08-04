
#sortie : 
# 1) size of confidence interval
# 2) coverage based on first ALASSO estimation (mean over non zero beta of proportion of beta* in iter)
# 3) coverage based on true parameter (mean over non zero beta of proportion of beta* in iter)
# 4) coverage : beta alasso in beta*  ? proportion of non zero beta
# 5) coverage : true beta in beta*  ? proportion of non zero beta
# 6) nb de faux nul 
# 7) mean 1st bias 
# 8) mean 2sd bias 


lahiriboot2<-function(data,iter,alpha,nonzero,post,betatrue){
  lass<-funboot(data,iter,post)
  las<-matrix(unlist(lass[1]),dim(data)[2]-1,iter)
  beta<-matrix(unlist(lass[2]))
  qnorm1<-qnorm(alpha/2,mean=0,sd=1)/sqrt(dim(data)[1])
  qnorm2<-qnorm(1-alpha/2,mean=0,sd=1)/sqrt(dim(data)[1])
  dist<-rep(0,nonzero)
  betastar<-rep(0,nonzero)
  cove1<-rep(0,nonzero)
  cove2<-rep(0,nonzero)
  cove3<-rep(0,nonzero)
  cove4<-rep(0,nonzero)
  fn<-rep(0,nonzero)
  
  for (j in 1:nonzero){
    dist[j]<-quantile(las[j,], 1-alpha/2)-quantile(las[j,], alpha/2)
    betastar[j]<-mean(las[j,])
    g<-NULL
    gtrue<-NULL
    fauxnegatif<-NULL
    for (i in (1:iter)){
      g[i]<-sum((las[j,i]<=(qnorm2+beta[j]))&(las[j,i]>=(qnorm1+beta[j])))
      gtrue[i]<-sum((las[j,i]<=(qnorm2+betatrue[j]))&(las[j,i]>=(qnorm1+betatrue[j])))
      fauxnegatif[i]<-sum((las[j,i]==0))
    }
    cove1[j]<-sum(g)/iter 
    cove2[j]<-sum(gtrue)/iter 
    cove3[j]<-sum((beta[j]<=quantile(las[j,], 1-alpha/2))&(beta[j]>=quantile(las[j,], alpha/2)))
    cove4[j]<-sum((betatrue[j]<=quantile(las[j,], 1-alpha/2))&(betatrue[j]>=quantile(las[j,], alpha/2)))
    fn[j]<-sum(fauxnegatif)
  }  
  distnonzero<-mean(dist)
  cover1<-mean(cove1)
  cover2<-mean(cove2)
  cover3<-mean(cove3)
  cover4<-mean(cove4)
  sumfn<-sum(fn)
  bias1<-mean(abs(betatrue)-abs(beta[1:nonzero]))
  bias2<-mean(abs(beta[1:nonzero])-abs(betastar))
  return(list(distnonzero,cover1,cover2,cover3,cover4,sumfn,bias1,bias2))
}