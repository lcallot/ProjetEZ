load("Agrege/Base/vardata.Rdata")

require('reshape2')
require('ggplot2')
library('lassovar')
#library('dplyr')
library('urca')
#library('stats')

source("Functions/forecastfunction.R")
source("Functions/bootlassovar.R")
source("Functions/bootlassovarprediction.R")
source("Functions/forecastlahiri.R")
#source("Functions/plotfunction.R")



#dput(names(var))
df<-var[,c("date", "YER", "PCR", "GCR", "ITR", "XTR", "MTR", "YED",  
           "GCD", "ITD", "XTD", "MTD", "YFD", "YIN", "WIN", "TIN", 
           "YFN",  "HICPSA", "URX", "STN", "LTN","POILU", 
           "PCOMU", "YWR","WRN",  "EEN", "EXR",  "M1",
           "M3", "ESI", "LIB", "PPI", "DJES")]
Ddf <- tail(df,-1) - head(df,-1)

df<-df[81:176,]
df$date<-NULL

df2<-matrix(0,92,32)
for (j in 1:32){
  for (i in 1:92){
    df2[i,j]<-df[i+4,j]-df[i,j]
  }
}
df2<-data.frame(df2)
colnames(df2)<-colnames(df)

df2$URX<-df$URX[-(1:4)]
df2$STN<-df$STN[-(1:4)]
df2$LTN<-df$LTN[-(1:4)]
df2$EEN<-df$EEN[-(1:4)]
df2$EXR<-df$EXR[-(1:4)]
df2$LIB<-df$LIB[-(1:4)]

# df2  : starts Q1 1991 ends Q4 2013



#Dsub <- tail(sub,-1) - head(sub,-1)



# bootlassovar
iter=100
adap="lasso"
lag=16
preforecast=28
horizon=12
TREND=FALSE



bootcoef<-bootlassovar(df2[1:56,],lag,iter,adap,TREND)
Q1<-bootlassovar.prediction(df2[1:56,],bootcoef,lag,preforecast,horizon,TREND)
#save(Q1,file="Agrege/Replication/Q1.Rdata")

name="HICPSA"
plotfunction(Q1,name,preforecast+horizon,iter,"no",4)
plot(x=seq(as.Date("1998/01/1"), as.Date("2007/12/1"), by = "quarter"),y=df2$HICPSA[29:68],type="l")

plot(df2$HICPSA)

plotfunction<-function(liste,name,lenght,iter,yoy,freq){
  
  e<-matrix(NA,lenght,iter)
  for (i in 1:iter){
    e[,i]<-liste[[i]][,name]
  }
  
  if (yoy=="yes"){
    f<-matrix(NA,lenght-freq,iter)
    for (j in 1:(lenght-freq)){
      f[j,]<-e[j+freq,]-e[j,]
    }
    
    df<-data.frame(f)
    time<-seq(as.Date("1999/1/1"), as.Date("2007/12/1"), by = "quarter")
    df$time<-time
    D=melt(df, id='time')
    a<-ggplot(D,aes(time,value, group=variable, color=variable))+geom_line()
    
  } else {
    
    df<-data.frame(e)
    time<-seq(as.Date("1998/1/1"), as.Date("2007/12/1"), by = "quarter")
    df$time<-time
    D=melt(df, id='time')
    a<-ggplot(D,aes(time,value, group=variable, color=variable))+geom_line()
  }
  
  return(a)
}




adap="lasso"
lag=12
preforecast=16
horizon=12
name="HICPSA"
alpha=0.05
TREND=FALSE
data=df2


foreca<-forecastlahiri(data,lag,preforecast,horizon,adap,TREND,name,alpha)
foreca<-data.frame(foreca)

time<-seq(as.Date("2011/1/1"), as.Date("2017/12/1"), by = "quarter")
foreca$time<-time
D=melt(foreca, id='time')
ggplot(D,aes(time,value, group=variable, color=variable))+geom_line()







