
funboot<-function(data,iter){
  estar=matrix(NA,length(data[,1]),iter)
  ystar=matrix(NA,length(data[,1]),iter)
  u=matrix(NA,dim(data)[2],iter)
  bsresvar <- rep(NA,iter)

    w<-(1/abs(lasso(y~.,data)$coef))[-1]
    if(sum(w!=Inf)==0) w<-NULL
    LASSO<-lasso( y ~ . , data=data, weights=w )
    coef<-(LASSO$coef)[-1]
    prediction<-as.matrix(LASSO$y-LASSO$res)
    residu<-(LASSO$res-mean(LASSO$res))
    for (i in 1:iter){
      estar[,i]<-as.matrix(sample(residu,length(residu),replace = T))
      ystar[,i]<-prediction+estar[,i]
      z<-ystar[,i]
      data2<-data.frame(z,data[,-1])
      v<-(1/abs(lasso(z~ . , data2)$coef))[-1]
      if(sum(v!=Inf)==0) v<-NULL
      lstar <- lasso(z ~ . , data2 , weights=v)
      u[,i]<-as.matrix(lstar$coef)
      bsresvar[i] <- var(lstar$res)
    } 
  return(list(u[-1,],coef,var(residu),bsresvar))
}
