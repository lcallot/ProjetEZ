library('glmnet')
library('MASS')

source("laurent/lasso.R")


# Simulated data
norm<-NULL
p=50
n=100
for (i in 1:p){
  norm<-cbind(norm,as.matrix(rnorm(n,0,1),n,p))
}
norm<-data.frame(norm)


rw=NULL
for (i in 1:p){
  rw<-cbind(rw,RW(n,0,0,1))
}
rw<-data.frame(rw)


fun<-function(df,iter,lasso){
  estar=matrix(NA,length(df[,1]),iter)
  ystar=matrix(NA,length(df[,1]),iter)
  u=matrix(NA,dim(df)[2],iter)
  a=NULL
  if (lasso==TRUE){
    a<-df[,1]
    LASSO<-lasso(a ~ . , df)
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
    } else {
    a<-df[,1]
    OLS<-lm(a~. , df)
    prediction<-as.matrix(OLS$fitted.value)
    eols<-OLS$res
    residu<-eols-mean(eols)
    for (i in 1:iter){
      estar[,i]<-as.matrix(sample(residu,length(residu),replace = T))
      ystar[,i]<-prediction+estar[,i]
      z<-ystar[,i]
      df2<-data.frame(z,df[,-1])
      boot<-lm(z ~ . , df2 )
      bootcoef<-boot$coef
      u[,i]<-as.matrix(bootcoef)
    }
  }
  return(u)
}

ols<-fun(norm,100,FALSE)
plot(density(ols[1,]))
plot(density(ols[2,]))
plot(density(ols[3,]))

lasso<-fun(norm,100,TRUE)
plot(density(lasso[1,]))
plot(density(lasso[2,]))
plot(density(lasso[3,]))


resultrw<-fun(c,d,10000,FALSE)
plot(density(resultrw[1,]))
plot(density(resultrw[2,]))


