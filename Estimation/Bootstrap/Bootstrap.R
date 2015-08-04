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


beta
df2<-iid5(p,beta,beta,beta,-beta,-beta,n)
betatrue=c(beta,beta,beta,-beta,-beta,rep(0,p-5))
prediction<-function(data,iter,betatrue){
  
  fun<-funboot(data,iter,"post")
  coef<-matrix(unlist(fun[1]),dim(df2)[2]-1,iter)
  x<-rnorm(p,0,1)
  y=rep(NA,iter)
  for (i in 1:iter){
    y[i]<-t(x)%*%coef[,i]
  }
  
}

iter=1000
p=50
beta=2
n=100



plot(density(y))
t(x)%*%c(beta,beta,beta,-beta,-beta,rep(0,p-5))


mcfunction<-function(p,nonzero,beta,n,beta,alpha){
  allzero<-TRUE
  while(allzero=TRUE){ 
  #1 : generate the data
  df<-iid5(p,beta,beta,beta,-beta,-beta,n)
  
  # estimate ALASSO
  allzero<-(sum(lasso(y~.,data)$coef!=0)=0)
    
  lahiriboot2(df,iter,alpha,nonzero,c(beta,beta,beta,-beta,-beta))
  }
  
}




