load("Agrege/Base/vardata.Rdata")

require('reshape2')
require('ggplot2')
library('lassovar')
#library('dplyr')
#library('urca')
#library('stats')

source("Functions/forecastfunction.R")
source("Functions/bootlassovar.R")

sub1<- var[,c('date','M3','STN','LTN','YED')]

# data from Q1 1990 to 2013 Q4 (81:176)
sub<-sub1[81:176,]
plot(sub$M3)

#foreca<-forecast(sub[,-1],1,12,16,"none",FALSE)
#foreca<-data.frame(foreca)
#var1<-foreca[,1:4]
#time<-seq(as.Date("2011/1/1"), as.Date("2017/12/1"), by = "quarter")
#var1$time<-time
#mvar1 <- melt(var1,  id = 'time', variable.name = 'series')
#ggplot(mvar1, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")

# postestimation ? 
lv<-lassovar(sub[,-1], lags=4, adaptive="lasso", post = TRUE, ncores =1)$post


# bootlassovar
iter=100
bootcoef<-bootlassovar(sub[,-1],4,iter,"lasso")


tryfunction<-function(data,bootcoefficient,lag,preforecast,horizon){

  pre<-list()
  for (j in 1:length(bootcoef)){
    intercept<-as.matrix(bootcoef[[j]][1,])
    coef<-as.matrix(bootcoef[[j]][2:(lag*dim(bootcoef[[j]])[2]+1),],
                    lag*dim(bootcoef[[j]])[2],dim(bootcoef[[j]])[2])
  
    fore<-matrix(0,nrow=dim(data)[2],ncol=horizon+preforecast)
    fore[,1:(preforecast)]<-t(data[(dim(data)[1]-preforecast+1):dim(data)[1],])
    
    for (i in (preforecast+1):(horizon+preforecast)){
      M<-NULL
      for (l in 1:lag){
        M<-c(fore[,(i-l)],M)
      }
      fore[,i]<-t(M%*%coef)+intercept
    }
    rownames(fore)<-names(data)
    pre[[j]]<-t(fore)
    
  }
  return(pre)
}

Q<-tryfunction(sub[,-1],bootcoef,4,16,16)


M3pred<-matrix(NA,32,iter)
for (i in 1:iter){
  M3pred[,i]<-Q[[i]][,1]
}
df<-data.frame(M3pred)
time<-seq(as.Date("2010/1/1"), as.Date("2017/12/1"), by = "quarter")
df$time<-time
D=melt(df, id='time')
ggplot(D,aes(time,value, group=variable, color=variable))+geom_line()
