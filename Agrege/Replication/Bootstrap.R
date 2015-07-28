library('glmnet')
library('MASS')

rw<-function(t,x,mean,var){
  y<-matrix(0,1,t)
  y[,1]<-x
    for(i in 2:t){
      y[,i] <- y[,i-1] + mvrnorm(n = 1, mu= mean , Sigma= var)
    }
  return(t(y))
}

# Simulated data

a<-rnorm(100,0,1)
b<-rnorm(100,0,1)
c<-rw(100,0,0,1)
d<-rw(100,0,0,1)

fun<-function(y,x,iter,lasso){
  estar=matrix(NA,length(y),iter)
  ystar=matrix(NA,length(y),iter)
  u=matrix(NA,2,iter)
  if (lasso==TRUE){
    
  } else {
    OLS<-lm(a~b)
    prediction<-as.matrix(OLS$fitted.value)
    coef<-OLS$coef
    eols<-OLS$res
    residu<-eols-mean(eols)
    for (i in 1:iter){
      estar[,i]<-as.matrix(sample(residu,length(residu),replace = T))
      ystar[,i]<-prediction+estar[,i]
      boot<-lm(ystar[,i]~x)
      bootcoef<-boot$coef
      u[,i]<-as.matrix(bootcoef)
    }
  }
  return(u)
}

result<-fun(a,b,10000,FALSE)

plot(density(result[1,]))
plot(density(result[2,]))


resultrw<-fun(c,d,1000,FALSE)
plot(density(resultrw[1,]))
plot(density(resultrw[2,]))







