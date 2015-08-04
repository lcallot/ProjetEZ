
lahiri<-function(data,alpha){
  w<-(1/abs(lasso(y~.,data)$coef))[-1]
  if(sum(w!=Inf)==0) w<-NULL
  res<-lasso(y~.,data,pweights=w)$res
  qnorm1<-qnorm(alpha/2,mean=0,sd=1)
  qnorm2<-qnorm(1-alpha/2,mean=0,sd=1)
  q1<-quantile(res, alpha/2)
  q2<-quantile(res, 1-alpha/2)
  dist<-q2-q1
  if ((q1>qnorm1) & (q2<qnorm2)){
    cover<-(q2-q1)/(qnorm2-qnorm1)
  } else {
    if ((q1<qnorm1) & (q2<qnorm2)){
      cover<-(q2-qnorm1)/(qnorm2-qnorm1)
    } else {
      if ((q1>qnorm1) & (q2>qnorm2)){
        cover<-(qnorm2-q1)/(qnorm2-qnorm1)
      } else {
        cover<-1
      }
    }
  }
  return(c(dist,cover))
}
