library('glmnet')
library('MASS')

source("laurent/lasso.R")
source("Functions/RW.R")
source("Functions/fun.R")
source("Functions/lahiri.R")


# Simulated data
norm<-NULL
p=20
n=100
for (i in 1:p){
  norm<-cbind(norm,as.matrix(rnorm(n,0,1),n,p))
}
beta<-matrix(c(1,-1,rep(0,p-2)))


# iid variable
y<-norm%*%beta+matrix(rnorm(n,0,1),n,1)
df<-data.frame(y,norm)

# lahiri article
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
CI(300,10,100)




# Simulated data
norm<-NULL
p=50
n=100
for (i in 1:p){
  norm<-cbind(norm,as.matrix(rnorm(n,0,1),n,p))
}
beta<-matrix(c(4,-1,rep(0,p-2)))


# iid variable
y<-norm%*%beta+matrix(rnorm(n,0,1),n,1)
df<-data.frame(y,norm)



# Lahiri Bootstrap
ols<-funboot(df,500,"ols")
ols[2]

las<-funboot(df,500,"alasso")


plot(density(ols[2,]))
plot(density(las[3,]))






norm<-NULL
for (i in 1:p){
  norm<-cbind(norm,as.matrix(rnorm(n,0,1),n,p))
}
beta<-matrix(c(1,1,1,-1,-1,-1,rep(0,p-nonzero)))

# iid variable
y<-norm%*%beta+matrix(rnorm(n,0,1),n,1)
df<-data.frame(y,norm)


lahiriboot(20,100,100,0.05,6)
data=df
iter=100
alpha=0.05
nonzero=6

lahiriboot<-function(data,iter,alpha,nonzero){
  
  lass<-funboot(data,iter,"alasso")
  las<-matrix(unlist(lass[1]),dim(data)[2]-1,iter)
  qnorm1<-qnorm(alpha/2,mean=0,sd=1)
  qnorm2<-qnorm(1-alpha/2,mean=0,sd=1)
  q1<-rep(0,dim(data)[2]-1)
  q2<-rep(0,dim(data)[2]-1)
  dist<-rep(0,dim(data)[2]-1)
  cover<-rep(0,dim(data)[2]-1)
  coef<-matrix(unlist(lass[2]))
  
  for (j in 1:(dim(data)[2]-1)){
    q1[j]<-quantile(las[j,], alpha/2)
    q2[j]<-quantile(las[j,], 1-alpha/2)
    dist[j]<-quantile(las[j,], 1-alpha/2)-quantile(las[j,], alpha/2)
    
    if ((q1[j]>qnorm1+coef[j]) & (q2[j]<qnorm2+coef[j])){
      cover[j]<-(q2[j]-q1[j])/(qnorm2-qnorm1)
    } else {
      if ((q1<qnorm1+coef[j]) & (q2[j]<qnorm2+coef[j])){
        cover[j]<-(q2[j]-(qnorm1+coef[j]))/(qnorm2-qnorm1)
      } else {
        if ((q1[j]>qnorm1) & (q2[j]>qnorm2)){
          cover[j]<-(qnorm2+coef[j]-q1[j])/(qnorm2-qnorm1)
        } else {
          cover[j]<-1
        }
      }
    }
    
  }
  
  distnonzero<-mean(dist[1:nonzero])
  distzero<-mean(dist[(1+nonzero):dim(data)[2]])
  covernonzero<-mean(cover[1:nonzero])
  coverzero<-mean(cover[(1+nonzero):dim(data)[2]])
  return(list(distnonzero,covernonzero,distzero,coverzero))
}






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






