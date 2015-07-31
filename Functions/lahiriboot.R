lahiriboot<-function(data,iter,alpha,nonzero){
  
  lass<-funboot(data,iter,"alasso")
  las<-matrix(unlist(lass[1]),dim(data)[2]-1,iter)
  beta<-matrix(unlist(lass[2]))
  qnorm1<-qnorm(alpha/2,mean=0,sd=1)/sqrt(dim(data)[1])
  qnorm2<-qnorm(1-alpha/2,mean=0,sd=1)/sqrt(dim(data)[1])
  q1<-rep(0,nonzero)
  q2<-rep(0,nonzero)
  dist<-rep(0,nonzero)
  cover<-rep(0,nonzero)
  
  for (j in 1:nonzero){
    q1[j]<-quantile(las[j,], alpha/2)
    q2[j]<-quantile(las[j,], 1-alpha/2)
    dist[j]<-q2[j]-q1[j]
    
    if ((q1[j]>(qnorm1+beta[j])) & (q2[j]<(qnorm2+beta[j]))){
      cover[j]<-(q2[j]-q1[j])/(qnorm2-qnorm1)
    } else {
      if ((q1[j]<(qnorm1+beta[j])) & (q2[j]<(qnorm2+beta[j]))){
        cover[j]<-(q2[j]-(qnorm1+beta[j]))/(qnorm2-qnorm1)
      } else {
        if ((q1[j]>(qnorm1+beta[j])) & (q2[j]>(qnorm2+beta[j]))){
          cover[j]<-(qnorm2+beta[j]-q1[j])/(qnorm2-qnorm1)
        } else {
          if ((q1[j]>(qnorm2+beta[j]))){
            cover[j]<-0
          } else {
            if ((q2[j]<(qnorm1+beta[j]))){
              cover[j]<-0
            } else {
                cover[j]<-1
          }
        }
      }
    }
  }
}
  distnonzero<-mean(dist)
  covernonzero<-mean(cover)
  return(list(distnonzero,covernonzero))
}