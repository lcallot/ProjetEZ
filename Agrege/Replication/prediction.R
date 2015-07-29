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



bootlassovar<-function(df,iter,lasso){
  estar=matrix(NA,dim(df)[1],dim(df)[2])
  ystar=matrix(NA,dim(df)[1],dim(df)[2])
  
  u=matrix(NA,dim(df)[2],iter)
  
  lv<-lassovar(sub[,-1],lags=2)
  res<-residuals(lv)
  pred<-lv$y-res
  
  residu<-NULL
  for (l in 1:dim(res)[2]){
    residu<-cbind(residu,res[,l]-mean(res[,l]))
  }
  
  
  
  for (i in 1:iter){
    
    for (j in 1:dim(residu)[1]){
      estar[j,]<-residu[sample(seq(from=1,to=dim(residu)[1],by=1),1,replace = T),]
      
      M<-NULL
      for (l in 1:dim(residu)[2]){
        ystar[j,l]<-
      }
      
    }
    
    
    for (i in (preforecast+1):(horizon+preforecast)){
      
      for (l in 1:lag){
        M<-c(fore[,(i-l)],M)
      }
      fore[,i]<-t(M%*%coef)+intercept+trend*(i+dim(data)[1]-(preforecast))
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



