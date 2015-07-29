library('glmnet')
library('MASS')

source("laurent/lasso.R")
source("Agrege/Replication/RW.R")
source("Agrege/Replication/fun.R")

# Simulated data
norm<-NULL
p=5
n=100
for (i in 1:p){
  norm<-cbind(norm,as.matrix(rnorm(n,0,1),n,p))
}
beta<-matrix(c(1,-1,rep(0,p-2)))

y<-norm%*%beta+

#rw=NULL
#for (i in 1:p){
 # rw<-cbind(rw,RW(n,0,0,1))
#}
#rw<-data.frame(rw)

df<-data.frame(y,norm)


# 
ols<-fun(df,500,FALSE)
las<-fun(df,500,TRUE)

# True distribution
X<-as.matrix(cbind(matrix(rep(1,n)),rw[,-1]))
true<-mvrnorm(n = 1000000, 0*rep(1,p), Sigma = diag(1,p) )
solve(t(X)%*%X)

plot(density(ols[3,]))
plot(density(las[3,]))
plot(density(true[2,]))

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






