
#sortie : 
# 1) size of confidence interval
# 2) coverage based on first ALASSO estimation 
# 6) nb de faux nul 
# 7) mean 1st bias 
# 8) mean 2sd bias 

# N : number of CI test 
# betatrue : vector of nonzero parameter
  

lahiriboot3<-function(data,p,nonzero,iter,betatrue,alpha,N){
  
  bet<-matrix(c(betatrue,rep(0,p-nonzero)))
  
  lass<-funboot(data,iter)
  las<-matrix(unlist(lass[1]),dim(data)[2]-1,iter)
  
  dist<-rep(NA,N)
  cov<-rep(NA,N)
  
  for (n in 1:N){
    
    x<-rnorm(dim(data)[2]-1,0,1)
    y<-rep(NA,iter)
    
    for (l in 1:iter){
      y[l]<-las[,l]%*%x
    }
    dist[n]<-quantile(y, 1-alpha/2)-quantile(y, alpha/2)
    cov[n]<-sum(((t(bet)%*%x)<=quantile(y, 1-alpha/2))&((t(bet)%*%x)>=quantile(y, alpha/2)))
  }
  
  distm<-mean(dist)
  covm<-mean(cov)
  return(list(distm,covm))
}


