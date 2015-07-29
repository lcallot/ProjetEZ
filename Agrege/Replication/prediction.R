load("Agrege/Base/vardata.Rdata")

require('reshape2')
require('ggplot2')
library('lassovar')
library('dplyr')
library('urca')
library('stats')

source("Agrege/Replication/forecastfunction.R")


sub1<- var[,c('date','M3','STN','LTN','YED')]

# data from Q1 1990 to 2013 Q4 (81:176)
sub<-sub1[81:176,]


foreca<-forecast(sub[,-1],1,12,16,"none",FALSE)
foreca<-data.frame(foreca)
var1<-foreca[,1:8]
time<-seq(as.Date("2011/1/1"), as.Date("2017/12/1"), by = "quarter")
var1$time<-time
mvar1 <- melt(var1,  id = 'time', variable.name = 'series')
ggplot(mvar1, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")



df<-sub[,-1]
iter=50

lag=2

bootlassovar<-function(df,iter,lasso){
  estar=matrix(NA,dim(df)[1]-lag,dim(df)[2])
  ystar=matrix(0,dim(df)[1],dim(df)[2])
  ystar[(1:lag),]<-as.matrix(df[(1:lag),])
  u=matrix(NA,dim(df)[2],iter)
  
  lv<-lassovar(sub[,-1],lags=2)
  res<-residuals(lv)
  pred<-lv$y-res
  coef<-as.matrix(lv$coefficients[2:(lag*dim(df)[2]+1),],lag*dim(df)[2],dim(df)[2])
  intercept<-matrix(lv$coefficients[1,],1, dim(df)[2])
  residu<-NULL
  for (l in 1:dim(res)[2]){
    residu<-cbind(residu,res[,l]-mean(res[,l]))
  }
  

  for (i in 1:iter){
    
    
    for (j in 1:dim(residu)[1]){
      estar[j,]<-residu[sample(seq(from=1,to=dim(residu)[1],by=1),1,replace = T),]
    }
    
    for (t in (lag+1):dim(df)[1]){
      
      for (l in 1:lag){
        ystar[t,]<-ystar[t,] + ystar[t-l,]%*%coef[(1+(l-1)*dim(df)[2]):(dim(df)[2]*l),]
      } 
      ystar[t,]<-intercept + ystar[t,]
      ystar[t,]<- ystar[t,] + estar[t-lag,]
    }
    
    lassovar(ystar,lags=lag)
  }
  
  
  
    
    help(sample)
    
    ystar[,i]<-prediction+estar[,i]
    z<-ystar[,i]
    df2<-data.frame(z,df[,-1])
    boot<-lasso(z ~ . , df2 )
    bootcoef<-boot$coef
    u[,i]<-as.matrix(bootcoef)
  }
  
  return(u)
}



help(lassovar)



