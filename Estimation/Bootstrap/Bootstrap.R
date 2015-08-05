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



################


mcfunction<-function(p,nonzero,beta,n,boot,alpha){
  allzero<-TRUE
  while(allzero==TRUE){ 
    #1 : generate the data
    if (type="iid1"){
      df<-iid1(p,beta,n)
    } else {
      if (type="iid5"){
        df<-iid5(p,beta,beta,beta,-beta,-beta,n)
      } else {
        if (type="iid10"){
          df<-iid5(p,beta,beta,beta,beta,beta,-beta,-beta,-beta,-beta,-beta,n)
        } else {
          if (type="AR1"){
            df<-depAR1(p,beta1,n)
          } else {
              df<-NULL
          } 
        }
      }
    }
    
    # estimate ALASSO
    allzero<-(sum((lasso(y~.,df)$coef[-1])!=0)==0)
    
    # Calculate the statistics
    result<-lahiriboot2(df,boot,alpha,nonzero,c(beta,beta,beta,-beta,-beta))
  }
  return(result)
}


MC<-function(iter,p,nonzero,beta,n,boot,alpha){
  a<-replicate(iter, mcfunction(p,nonzero,beta,n,boot,alpha))
  res<-list()
  for (i in 1:5){
    res[[i]]<-unlist(a[i,]) 
  }
  return(res)
}

iter=100
boot=100
n=100
beta=1
p=50
lapply(MC(iter,p,5,beta,n,boot,0.1), FUN = "mean" )






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















