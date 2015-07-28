df<-norm
bootlassovar<-function(df,iter,lasso){
  estar=matrix(NA,length(df[,1]),iter)
  ystar=matrix(NA,length(df[,1]),iter)
  u=matrix(NA,dim(df)[2],iter)

    LASSO<-lasso( X1 ~ . , df)
    prediction<-as.matrix(LASSO$y-LASSO$residuals)
    elasso<-LASSO$res
    residu<-elasso-mean(elasso)
    for (i in 1:iter){
      estar[,i]<-as.matrix(sample(residu,length(residu),replace = T))
      ystar[,i]<-prediction+estar[,i]
      z<-ystar[,i]
      df2<-data.frame(z,df[,-1])
      boot<-lasso(z ~ . , df2 )
      bootcoef<-boot$coef
      u[,i]<-as.matrix(bootcoef)
    }

  return(u)
}


