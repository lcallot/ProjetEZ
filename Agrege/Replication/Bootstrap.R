library('glmnet')
library('MASS')

source("laurent/lasso.R")
source("Agrege/Replication/RW.R")
source("Agrege/Replication/fun.R")

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




# 
ols<-fun(rw,5000,FALSE)
las<-fun(rw,5000,TRUE)

# True distribution
X<-as.matrix(cbind(matrix(rep(1,n)),rw[,-1]))
true<-mvrnorm(n = 1000000, 0*rep(1,p), Sigma = solve(t(X)%*%X) )


plot(density(ols[2,]))
plot(density(las[2,]))
plot(density(true[2,]))







