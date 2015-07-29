library('glmnet')
library('MASS')

source("laurent/lasso.R")
source("Agrege/Replication/RW.R")
source("Agrege/Replication/fun.R")

# Simulated data
norm<-NULL
p=10
n=100
for (i in 1:p){
  norm<-cbind(norm,as.matrix(rnorm(n,0,1),n,p))
}
beta<-matrix(c(1,-1,rep(0,p-2)))


# iid variable
y<-norm%*%beta+matrix(rnorm(n,0,1),n,1)
df<-data.frame(y,norm)

# dependant variable 
dep<-function(a){
  z<-rep(0,n)
  z[1]<-rnorm(1,0,1)
  for (i in 2:n){
    z[i]<-a*z[i-1]+rnorm(1,0,1)
  }
  return(z)
}

# alpha = 0.5 
z<-dep(0.5)
df2<-data.frame(z,norm)

# alpha = 0.8 
z<-dep(0.8)
df3<-data.frame(z,norm)

# alpha = 1 
z<-dep(1)
df4<-data.frame(z,norm)

plot(z)


# 
ols<-funboot(df4,500,"ols")
las<-funboot(df4,500,"lasso")


plot(density(ols[2,]))
plot(density(las[2,]))










ciols=NULL
cilasso=NULL
test=NULL
testlas=NULL
testols=NULL
for (j in 1:p){
  ciols=c(quantile(ols[j,], 0.025),quantile(ols[j,], 0.975))
  cilasso=c(quantile(las[j,], 0.025),quantile(las[j,], 0.975))
  test[j]<-((cilasso[1]>ciols[1])&(cilasso[2]<ciols[2])) 
  testlas[j]<-((cilasso[1]<=0)&(cilasso[2]>=0)) 
  testols[j]<-((0>=ciols[1])&(0<=ciols[2])) 
}
test
testlas
testols






