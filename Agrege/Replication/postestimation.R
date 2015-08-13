load("Agrege/Base/vardata.Rdata")

require('reshape2')
require('ggplot2')
library('lassovar')
#library('dplyr')
#library('urca')
#library('stats')

#source("Functions/forecastfunction.R")
#source("Functions/bootlassovar.R")
#source("Functions/bootlassovarprediction.R")
#source("Functions/plotfunction.R")
source("Functions/ci.R")
source("Functions/forecastci.R")


#dput(names(var))
df<-var[,c("date", "YER", "PCR", "GCR", "ITR", "XTR", "MTR", "YED",  
           "GCD", "ITD", "XTD", "MTD",  "WIN",  
           "HICPSA", "URX", "STN", "LTN","POILU", 
           "PCOMU", "YWR","WRN",  "EEN", "EXR",  "M1",
           "M3", "ESI", "LIB", "PPI", "DJES")]

#df<-var[,c("date", "YER", "PCR", "GCR")]

Ddf <- tail(df,-1) - head(df,-1)

df<-df[81:176,]
df$date<-NULL

df2<-matrix(0,92,dim(df)[2])
for (j in 1:dim(df)[2]){
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


lag=8
preforecast=12
horizon=12
adap="none"
TREND=FALSE
name="HICPSA"
alpha=0.1
post="yes"

foreca<-forecastci(df2,lag,preforecast,horizon,adap,TREND,name,alpha,post)
foreca<-data.frame(foreca)
time<-seq(as.Date("2011/1/1"), as.Date("2016/12/1"), by = "quarter")
foreca$time<-time
D=melt(foreca, id='time')
ggplot(D,aes(time,value, group=variable, color=variable))+geom_line()


