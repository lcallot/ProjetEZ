load("data/vardatam.RData")

require('reshape2')
require('ggplot2')
library('lassovar')
#library('dplyr')
library('urca')
#library('stats')

source("Functions/forecastfunction.R")
source("Functions/bootlassovar.R")
source("Functions/bootlassovarprediction.R")

df<- vardatam
df<- df[85:288,]
#df<-var
# data from Q4 1989 to 2013 Q4 (80:176)
#df<-sub1[80:176,]
df$time<-NULL
df<-df[,(1:109)]
#erstest<-function(data,trend,type,lagmax){
# result<-NULL
#for (i in seq(1,dim(data)[2],1)){
# ers<-ur.ers(data[,i], type = type, model=trend, lag.max = lagmax)
#stat<-ers@teststat
#value5pc<-ers@cval[2]
#result[i]<-stat>value5pc
#}
#return(rbind(names(data),result))
#}
#erstest(df[,-1],"trend","P-test",10)



#Ddf <- tail(df[,-1],-1) - head(df[,-1],-1)

#data<-Ddf
#data$STN<-df$STN[-1]
#data$EEN<-df$EEN[-1]
#data$EXR<-df$EXR[-1]


#plot(df$HICP_EZ)
#x<-df$HICP_EZ
#z<-NULL
#for (i in 5:length(x)){
#  z[i]<-x[i]-x[i-4]
#}

#plot(100*z[-(1:4)],type="l")

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
iter=10
adap="lasso"
lag=1
preforecast=28
horizon=16

bootcoef<-bootlassovar(df,lag,iter,adap)
Q<-bootlassovar.prediction(df,bootcoef,lag,preforecast,horizon)

name="HICP_EZ"
liste=Q
lenght=preforecast+horizon
freq=4

tryfun<-function(liste,name,lenght,iter,yoy,freq){
  
  e<-matrix(NA,lenght,iter)
  for (i in 1:iter){
    e[,i]<-liste[[i]][,name]
  }
  
  if (yoy=="yes"){
    f<-matrix(NA,lenght-freq,iter)
    for (j in 1:(lenght-freq)){
      f[j,]<-e[j+freq,]-e[j,]
    }
    
    df<-data.frame(f[-(1:freq),])
    time<-seq(as.Date("2009/1/1"), as.Date("2017/12/1"), by = "quarter")
    df$time<-time
    D=melt(df, id='time')
    a<-ggplot(D,aes(time,value, group=variable, color=variable))+geom_line()
    
  } else {
    
    df<-data.frame(e)
    time<-seq(as.Date("2008/1/1"), as.Date("2017/12/1"), by = "quarter")
    df$time<-time
    D=melt(df, id='time')
    a<-ggplot(D,aes(time,value, group=variable, color=variable))+geom_line()
  }
  
  return(a)
}
tryfun(Q,"HICP_EZ",preforecast+horizon,iter,"yes",4)


