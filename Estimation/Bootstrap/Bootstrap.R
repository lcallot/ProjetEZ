library('glmnet')
library('MASS')

source("laurent/lasso.R")
source("Functions/RW.R")
source("Functions/fun.R")
source("Functions/lahiri.R")
source("Functions/lahiriboot.R")

source("Functions/edf.R")

source("Functions/lahiriboot2.R")
source("Functions/datagen.R")

source("Functions/montecarlo.R")

################






iter=10
boot=10
n=100
beta=1
p=50
lapply(MC(iter,p,5,beta,n,boot,0.1,"AR1"), FUN = "mean" )






###################



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















