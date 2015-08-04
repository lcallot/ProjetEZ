library('glmnet')
library('MASS')

source("laurent/lasso.R")
source("Functions/RW.R")
source("Functions/fun.R")
source("Functions/lahiri.R")
source("Functions/lahiriboot.R")
source("Functions/AR1.R")
source("Functions/AR5.R")
source("Functions/edfAR1.R")
source("Functions/edfiid4.R")
source("Functions/edfiid1.R")
source("Functions/iid1.R")
source("Functions/iid5.R")
source("Functions/iid10.R")
source("Functions/lahiriboot2.R")

## lahiri  edf 

edfAR1(50,0.9,100,100,0.05)

edfiid1(150,1,4,100,1000,0.05)

edfiid5(150,4,4,2,-5,1,100,1000,0.05)



# Simulated data

# iid variable
df1<-iid1(10,0.3,100)
df2<-iid5(10,0.2,4,5,-3,-1,100)
df3<-depAR1(10,0.9,100)

df4<-depAR5(10,3,-3.55,2.07,-0.5944,0.0672,100)
lahiriboot2(df4,100,0.05,5,"post",c(3,-3.55,2.07,-0.5944,0.0672))

b3<-lahiriboot2(iid10(120,beta,beta,beta,beta,beta,-beta,-beta,-beta,-beta,-beta,100),iter,0.05,nonzero2,"post",betatrue2)

funboot(df3,10,"post")

# lahiri bootstrap

df2<-iid5(10,1,1,-1,1,-1,1000)
lahiriboot2(df2,1000,0.05,5,"post",c(1,1,-1,1,-1))
#sortie : 
# 1) size of confidence interval
# 2) coverage based on first ALASSO estimation (mean over non zero beta of proportion of beta* in iter)
# 3) coverage based on true parameter (mean over non zero beta of proportion of beta* in iter)
# 4) coverage : beta alasso in beta*  ? proportion of non zero beta
# 5) coverage : true beta in beta*  ? proportion of non zero beta
# 6) nb de faux nul 
# 7) mean 1st bias 
# 8) mean 2sd bias 

beta=1
df<-iid5(200,beta,beta,-beta,-beta,beta,100)
data=df
iter=100

a4<-lahiriboot2(df,iter,0.05,5,"post",betatrue1)
funboot<-function(data,iter,post){
  estar=matrix(NA,length(data[,1]),iter)
  ystar=matrix(NA,length(data[,1]),iter)
  u=matrix(NA,dim(data)[2],iter)
  if (post=="post"){
    w<-(1/abs(lasso(y~.,data)$coef))[-1]
    if(sum(w!=Inf)==0) w<-NULL
    LASSO<-lasso( y ~ . , data=data, weights=w )
    coef<-(LASSO$coef)[-1]
    prediction<-as.matrix(LASSO$y-LASSO$post$res)
    residu<-(LASSO$res-mean(LASSO$res))
    for (i in 1:iter){
      estar[,i]<-as.matrix(sample(residu,length(residu),replace = T))
      ystar[,i]<-prediction+estar[,i]
      z<-ystar[,i]
      data2<-data.frame(z,data[,-1])
      v<-(1/abs(lasso(z~ . , data2)$coef))[-1]
      if(sum(v!=Inf)==0) v<-NULL
      u[,i]<-as.matrix(lasso(z ~ . , data2 , weights=v)$coef)
    }
  } else {
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
      u[,i]<-as.matrix(lasso(z ~ . , data2 , weights=v)$coef)
    }
  }
  return(list(u[-1,],coef))
}


