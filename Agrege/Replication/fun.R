
fun<-function(df,iter,las){
  estar=matrix(NA,length(df[,1]),iter)
  ystar=matrix(NA,length(df[,1]),iter)
  u=matrix(NA,dim(df)[2],iter)
  if (las==TRUE){
    LASSO<-lasso( y ~ . , df)
    prediction<-as.matrix(LASSO$y-LASSO$residuals)
    residu<-LASSO$res-mean(LASSO$res)
    for (i in 1:iter){
      estar[,i]<-as.matrix(sample(residu,length(residu),replace = T))
      ystar[,i]<-prediction+estar[,i]
      z<-ystar[,i]
      df2<-data.frame(z,df[,-1])
      boot<-lasso(z ~ . , df2 )
      u[,i]<-as.matrix(boot$coef)
    }
  } else {
    OLS<-lm( y ~ . , df)
    prediction<-as.matrix(OLS$fitted.value)
    residu<-OLS$res-mean(OLS$res)
    for (i in 1:iter){
      estar[,i]<-as.matrix(sample(residu,length(residu),replace = T))
      ystar[,i]<-prediction+estar[,i]
      z<-ystar[,i]
      df2<-data.frame(z,df[,-1])
      boot<-lm(z ~ . , df2 )
      u[,i]<-as.matrix(boot$coef)
    }
  }
  return(u)
}

