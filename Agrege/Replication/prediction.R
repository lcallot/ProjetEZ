load("Agrege/Base/vardata.Rdata")

require('reshape2')
require('ggplot2')
library('lassovar')
#library('dplyr')
#library('urca')
#library('stats')

source("Functions/forecastfunction.R")
source("Functions/bootlassovar.R")
source("Functions/bootlassovarprediction.R")

df<- var[,c('date','M3','STN','HICP_EZ','URX','POILU','EEN','EXR','LFI','LHO')]
df<- df[112:176,]

# data from Q4 1989 to 2013 Q4 (80:176)
#df<-sub1[80:176,]
#df$date<-NULL



Ddf <- tail(df[,-1],-1) - head(df[,-1],-1)
Ddf$HICP_EZ<-4*Ddf$HICP_EZ


plot(Ddf$HICP_EZ,type="l")
plot(Ddf,type="l")


DF<-data.frame(Ddf$M3,df$STN[-1],Ddf$HICP_EZ,Ddf$URX,Ddf$POILU,df$EEN[-1],df$EXR[-1],Ddf$LFI,Ddf$LHO)
colnames(DF)<-names(df[,-1])


#foreca<-forecast(sub[,-1],1,12,16,"none",FALSE)
#foreca<-data.frame(foreca)
#var1<-foreca[,1:4]
#time<-seq(as.Date("2011/1/1"), as.Date("2017/12/1"), by = "quarter")
#var1$time<-time
#mvar1 <- melt(var1,  id = 'time', variable.name = 'series')
#ggplot(mvar1, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")

# postestimation ? 
#lv<-lassovar(sub[,-1], lags=4, adaptive="lasso", post = TRUE, ncores =1)$post


# bootlassovar
iter=20
adap="none"
lag=4

bootcoef<-bootlassovar(DF,lag,iter,adap)



# bootlassovar.prediction
preforecast=16
horizon=16

Q<-bootlassovar.prediction(DF,bootcoef,lag,preforecast,horizon)


tryfun<-function(data,name,lenght,iter){
  e<-matrix(NA,32,iter)
  for (i in 1:iter){
    e[,i]<-Q[[i]][,name]
  }
  df<-data.frame(e)
  time<-seq(as.Date("2010/1/1"), as.Date("2017/12/1"), by = "quarter")
  df$time<-time
  D=melt(df, id='time')
  a<-ggplot(D,aes(time,value, group=variable, color=variable))+geom_line()
  return(a)
}
tryfun(Q,"HICP_EZ",32,iter)




