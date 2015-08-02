library('glmnet')
library('MASS')

source("laurent/lasso.R")
source("Functions/RW.R")
source("Functions/fun.R")
source("Functions/lahiri.R")
source("Functions/lahiriboot.R")

# Simulated data
norm<-NULL
p=10
n=100
nonzero=4

for (i in 1:p){
  norm<-cbind(norm,as.matrix(rnorm(n,0,1),n,1))
}
beta<-matrix(c(3,3,-1,-1,rep(0,p-nonzero)))

# iid variable
y<-norm%*%beta+matrix(rnorm(n,0,1),n,1)
df<-data.frame(y,norm)

#a<-funboot(df,100,"alasso")
#plot(density(matrix(unlist(a[1]),p,n)[1,]))
#unlist(a[2])[1]




# lahiri bootstrap
lahiriboot(df,100,0.05,nonzero)[1]


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







# dependant variable 
dep<-function(alpha,t,p,nonzero){
  beta=matrix(c(alpha,rep(0,p-nonzero)))
  z<-NULL
  z[1]<-rnorm(1,0,1)
  for (i in 2:(t+p)){
    z[i]<-beta[1]*z[i-1]+rnorm(1,0,1)
  }
  
  for (k in 1:(p+1)){
    Z<-matrix(0,t,p+1)
    Z[k,]<-z[(p+1-(k-1)):(t+p+1-k)]
  }
  
  return(Z)
}
Z<-dep(0.1,100,10,1)




# alpha = 0.5 
z<-dep(0.5)
df2<-data.frame(z,norm)

# alpha = 0.8 
z<-dep(0.8)
df3<-data.frame(z,norm)

# alpha = 1 
z<-dep(1)
df4<-data.frame(z,norm)
