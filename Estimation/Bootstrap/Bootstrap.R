library('glmnet')
library('MASS')

source("laurent/lasso.R")
source("Functions/RW.R")
source("Functions/fun.R")
source("Functions/lahiri.R")
source("Functions/lahiriboot.R")
source("Functions/AR1.R")
source("Functions/edfAR4.R")
source("Functions/edfAR1.R")
source("Functions/edfiid4.R")
source("Functions/edfiid1.R")
source("Functions/AR4.R")


## lahiri  edf 
depAR1(10,1,0.9,100)






# Simulated data

# iid variable
norm<-NULL
p=10
n=100
nonzero=1
for (i in 1:p){
  norm<-cbind(norm,as.matrix(rnorm(n,0,1),n,1))
}
beta<-matrix(c(0.4,rep(0,p-nonzero)))
y<-norm%*%beta+matrix(rnorm(n,0,1),n,1)
df<-data.frame(y,norm)


# dependant variable 
depAR1<-function(alpha,t,p,nonzero){
  beta=matrix(c(alpha,rep(0,p-nonzero)))
  z<-NULL
  z[1]<-rnorm(1,0,1)
  for (i in 2:(t+p)){z[i]<-beta[1]*z[i-1]+rnorm(1,0,1)}
  Z<-matrix(0,t,p+1)
  for (k in 1:(p+1)){Z[,k]<-matrix(z[(p+1-(k-1)):(t+p+1-k)])}
  return(Z)
}
Z<-dep(0.4,100,10,1)
y<-Z[,1]
df2<-data.frame(cbind(y,Z[,-1]))


# lahiri bootstrap
lahiriboot(df,1000,0.05,nonzero)


# lahiri empirical edf
lahiri(df,0.05)






