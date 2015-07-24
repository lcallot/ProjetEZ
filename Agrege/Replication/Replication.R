require('reshape2')
require('ggplot2')
library('lassovar')
library('dplyr')
library('urca')
library('stats')

load("Agrege/Base/vardata.Rdata")

sub1<- var[,c('date','M1','M3','STN','LTN','YED','YER')]

# data from Q1 1990 to 2013 Q4 (81:176)
sub<-sub1[81:176,]

# dif data from Q1 1990 to 2013 Q4
sub2<-sub1[80:176,]
dsub <- tail(sub2[,-1],-1) - head(sub2[,-1],-1)
date<-sub2[-1,1]
dsub<-data.frame(date,dsub)

# dif dif data from Q1 1990 to 2013 Q4
sub3<-sub1[79:176,]
dsub2 <- tail(sub3[,-1],-1) - head(sub3[,-1],-1)
ddsub <- tail(dsub2,-1) - head(dsub2,-1)
ddsub<-data.frame(date,ddsub)

rownames(sub)<-NULL
rownames(dsub)<-NULL
rownames(ddsub)<-NULL

# type = P-test  for ERS  &  DF-GLS 
erstest<-function(data,trend,type,lagmax){
  result<-NULL
  for (i in seq(1,dim(data)[2],1)){
    ers<-ur.ers(data[,i], type = type, model=trend, lag.max = lagmax)
    stat<-ers@teststat
    value5pc<-ers@cval[2]
    result[i]<-stat>value5pc
  }
  return(rbind(names(data),result))
}


erstest(sub[,-1],"trend","P-test",10)
erstest(dsub[,-1],"constant","P-test",10)
erstest(ddsub[,-1],"constant","P-test",10)



#### Replication 

## Johansen test
RM3 <- sub$M3-sub$YED
Spread <- sub$LTN - sub$STN
pi<-4*dsub$YED
GDP <- sub$YER
Z=data.frame(RM3,Spread,pi,GDP)
Z<-as.ts(Z)
summary(ca.jo(Z,type="trace",ecdet="trend",K=2))

help(ca.jo)


# Long term relation estimation


reg<-lm(RM3 ~ GDP + Spread + pi + 0)
summary(reg)
res<-reg$residuals




# ECM
dRM3 <- dsub$M3-dsub$YED
dRS<-dsub$STN
dRL<-dsub$LTN
dpi<-4*ddsub$YED
dGDP <- dsub$YER
ddGDP <- ddsub$YER


a<-(dRS[-(1:2)] + lag(dRS)[-(1:2)])/2
b<-(dpi[-(1:2)]+lag(dpi)[-(1:2)])/2

ecm<-lm(dRM3[-(1:2)] ~ ddGDP[-(1:2)] + a + b +dRL[-(1:2)] + lag(res,lag=2)[-(1:2)])
summary(ecm)

ecm$res

edf<-function(x,alpha){
  y<-NULL
  y[i]<-(length(x)^-1)
}


edf<-ecdf(ecm$res)
plot.ecdf(edf)
summary(edf)
help(ecdf)




