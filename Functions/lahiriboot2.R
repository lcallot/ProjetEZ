
#sortie : 
# 1) size of confidence interval
# 2) coverage based on first ALASSO estimation 
# 6) nb de faux nul 
# 7) mean 1st bias 
# 8) mean 2sd bias 


lahiriboot2<-function(data,iter,alpha,nonzero,betatrue){

  lass<-funboot(data,iter)
  las<-matrix(unlist(lass[1]),dim(data)[2]-1,iter)
  beta<-matrix(unlist(lass[2]))

  dist<-rep(0,nonzero)
  betastar<-rep(0,nonzero)
  cove1 <- covt <- rep(0,nonzero)

  fn<-rep(0,nonzero)
  
  for (j in 1:nonzero){
    dist[j]<-quantile(las[j,], 1-alpha/2)-quantile(las[j,], alpha/2)
    betastar[j]<-mean(las[j,])

    fauxnegatif<-NULL
    for (i in (1:iter)){
      fauxnegatif[i]<-sum((las[j,i]==0))
    }
    
    #t-stat coverage
    that <- (beta[j]-betatrue[j])/sqrt(lass[[3]])
    tstar <- (las[j,]-beta[j])/sqrt(lass[[4]])
    qt1 <- quantile(tstar,1-alpha/2)
    qt2 <- quantile(tstar,alpha/2)
    covt[j] <- (that<=qt1)&(that>=qt2) 
    
    # std coverage
    cove1[j]<-sum((betatrue[j]<=quantile(las[j,]-mean(las[j,])+beta[j], 1-alpha/2))&(betatrue[j]>=quantile(las[j,]-mean(las[j,])+beta[j], alpha/2)))
    fn[j]<-sum(fauxnegatif)
  }  
  
  distnonzero<-mean(dist)
  cover1<-mean(cove1)
  sumfn<-sum(fn)
  bias1<-mean(abs(betatrue)-abs(beta[1:nonzero]))
  bias2<-mean(abs(beta[1:nonzero])-abs(betastar))
  return(c(distnonzero,cover1,sumfn,bias1,bias2,mean(covt)))
}