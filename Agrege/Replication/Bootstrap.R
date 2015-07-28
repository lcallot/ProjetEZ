library('glmnet')
# Simulated data

a<-rnorm(50,2,1)
b<-rnorm(50,3,1)




OLS<-lm(a~b)
predols<-as.matrix(OLS$fitted.value)
coefols<-OLS$coef
eols<-OLS$res
etildols<-eols-mean(eols)

iter<-10000
t=matrix(NA,2,iter)
estar=matrix(NA,length(etildols),iter)
ystar=matrix(NA,length(etildols),iter)
u=matrix(NA,2,iter)


fun<-function(residu,prediction,coefficient,y,x,iter){
  OLS<-lm(a~b)
  predols<-as.matrix(OLS$fitted.value)
  coefols<-OLS$coef
  eols<-OLS$res
  etildols<-eols-mean(eols)
  
  
  LASSO<-lasso(a~b)
  predols<-as.matrix(OLS$fitted.value)
  coefols<-OLS$coef
  eols<-OLS$res
  etildols<-eols-mean(eols)
  
  
  t=matrix(NA,2,iter)
  estar=matrix(NA,length(etildols),iter)
  ystar=matrix(NA,length(etildols),iter)
  u=matrix(NA,2,iter)
  
  for (i in 1:iter){
    
    estar[,i]<-as.matrix(sample(residu,length(residu),replace = T))
    ystar[,i]<-prediction+estar[,i]
    
    boot<-lm(ystar[,i]~x)
    bootcoef<-boot$coef
    t[,i]<-as.matrix(bootcoeflasso)
    u[,i]<-as.matrix(bootcoefols)
  }
  return(u)
}
result<-fun(etildols,predols,coefols,a,b,1000)

plot(density(result[1,]))
plot(density(result[2,]))






