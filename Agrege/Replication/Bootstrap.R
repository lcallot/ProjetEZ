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

ciols=NULL
cilasso=NULL
test=NULL
for (j in 1:p){
  ciols=c(quantile(ols[p,], 0.025),quantile(ols[p,], 0.975))
  cilasso=c(quantile(ols[p,], 0.025),quantile(ols[p,], 0.975))
  test[j]<-
}

ciols=c(quantile(ols[2,], 0.025),quantile(ols[2,], 0.975))
cilasso=c(quantile(las[2,], 0.025),quantile(las[2,], 0.975))

is.element(cilasso, ciols) 

is.element(as.vector(0.0), set=seq(-1, 1, by = .001)) 

( 0.001 %in% seq(-1, 1, by = .001))
c=seq(-1, 1, by = .001)
sum(c==.100)

