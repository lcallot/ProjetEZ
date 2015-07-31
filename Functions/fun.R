
funboot<-function(data,iter,las){
  estar=matrix(NA,length(data[,1]),iter)
  ystar=matrix(NA,length(data[,1]),iter)
  u=matrix(NA,dim(data)[2],iter)
  if (las=="alasso"){
    w<-(1/abs(lasso(y~.,data)$coef))[-1]
    LASSO<-lasso( y ~ . , data=data, weights=w )
    coef<-(LASSO$coef)[-1]
    prediction<-as.matrix(LASSO$y-LASSO$residuals)
    residu<-(LASSO$res-mean(LASSO$res))
    for (i in 1:iter){
      estar[,i]<-as.matrix(sample(residu,length(residu),replace = T))
      ystar[,i]<-prediction+estar[,i]
      z<-ystar[,i]
      data2<-data.frame(z,data[,-1])
      v<-(1/abs(lasso(z~ . , data2)$coef))[-1]
      u[,i]<-as.matrix(lasso(z ~ . , data2 , weights=v)$coef)
    }
  } else {
    OLS<-lm( y ~ . , data)
    coef<-(OLS$coef)[-1]
    prediction<-as.matrix(OLS$fitted.value)
    residu<-OLS$res-mean(OLS$res)
    for (i in 1:iter){
      estar[,i]<-as.matrix(sample(residu,length(residu),replace = T))
      ystar[,i]<-prediction+estar[,i]
      z<-ystar[,i]
      data2<-data.frame(z,data[,-1])
      u[,i]<-as.matrix(lm(z ~ . , data2 )$coef)
    }
  }
  return(list(u[-1,],coef))
}










