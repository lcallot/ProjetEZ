library('glmnet')
library('MASS')

source("laurent/lasso.R")
source("Functions/RW.R")
source("Functions/fun.R")
source("Functions/lahiri.R")
source("Functions/lahiriboot.R")
source("Functions/AR1.R")

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
dep<-function(alpha,t,p,nonzero){
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

        # Replicate iter fois 
CI<-function(p,n,iter){
  beta<-matrix(c(1,1,1,-1,-1,-1,rep(0,p-6)))
  size<-rep(0,iter)
  cov<-rep(0,iter)
  for (j in 1:iter){
    norm<- matrix(rnorm(n*p,0,1),n,p)
    y<-norm%*%beta+matrix(rnorm(n,0,1),n,1)
    df<-data.frame(y,norm)
    size[j]<-lahiri(df,0.05)[1]
    cov[j]<-lahiri(df,0.05)[2]
  }
  v1<-mean(size)
  v2<-mean(cov)
  return(c(v1,v2))
}
CI(180,100,1000)









lahiri(df,0.05)


# dep : AR 
# alpha : CI
# gamma : parametre de l'AR(1)
CI<-function(p,n,iter,AR,gamma,alpha,nonzero){
  size<-rep(0,iter)
  cov<-rep(0,iter)
  if (TRUE==AR){
    for (j in 1:iter){
    Z<-dep(gamma,n,p,nonzero)
    y<-Z[,1]
    df<-data.frame(cbind(y,Z[,-1]))
    size[j]<-lahiri(df,alpha)[1]
    cov[j]<-lahiri(df,alpha)[2]
    }
  } else {
    beta<-matrix(c(1,rep(0,p-nonzero)))
    for (j in 1:iter){
      norm<- matrix(rnorm(n*p,0,1),n,p)
      y<-norm%*%beta+matrix(rnorm(n,0,1),n,1)
      df<-data.frame(y,norm)
      size[j]<-lahiri(df,alpha)[1]
      cov[j]<-lahiri(df,alpha)[2]
    }
  }
  v1<-mean(size)
  v2<-mean(cov)
  return(c(v1,v2))
}
CI(10,100,100,TRUE,0.1,0.05,1)


