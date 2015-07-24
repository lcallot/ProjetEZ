
require('reshape2')
require('ggplot2')
library('lassovar')
library('dplyr')
library('urca')

load("Agrege/Base/vardata.Rdata")

sub<- var[,c('date','M1','M3','STN','LTN','YED','YER')]

# data from Q1 1990 to 2013 Q4
sub<-sub[81:176,]

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
erstest(sub[,-1],"trend","DF-GLS",10)


# dif data from Q1 1990 to 2013 Q4
sub2<- var[,c('date','M1','M3','STN','LTN','YED','YER')]
sub2<-sub2[80:176,]
dsub <- tail(sub2[,-1],-1) - head(sub2[,-1],-1)
date<-sub2[-1,1]
dsub<-data.frame(date,dsub)
rownames(dsub)<-NULL

erstest(dsub[,-1],"constant","P-test",10)
erstest(dsub[,-1],"constant","DF-GLS",10)



#### Replication 

RealMon <- sub$M3-sub$YED
Spread <- sub$LTN - sub$STN
pi<-4*dsub$YED
GDP <- sub$YER

reg<-lm(RealMon ~ GDP + Spread + pi)
summary(reg)

library('tseries')
## Phillips-Ouliaris
z=c(RealMon,Spread,pi,GDP)
Z=matrix(z,nrow=96 , ncol=4)
po.test(Z, demean = TRUE, lshort = FALSE) 

##### Test de Johansen
Z1<-as.ts(Z)
summary(ca.jo(Z1,type="trace",ecdet="trend",K=2))






#regression de lrev sur lconso
reg<-lm(C~R)
summary(reg)

## recuperation des residus estimes
residu<-reg$residuals


#ADF
#Modele 3:
a=ur.df(residu, type = "trend", lags = 10, selectlags ="BIC");summary(a)

#Modele 2:
a=ur.df(residu, type = "drift", lags = 10, selectlags ="BIC");summary(a)

#Modele 1:
a=ur.df(residu, type = "none", lags = 10, selectlags ="BIC");summary(a)

library(fUnitRoots)
unitrootTest(residu, lags=1,type="ct")
unitrootTest(residu, lags=1,type="c")
unitrootTest(residu, lags=1,type="nc")

unitrootTable(trend="ct",statistic="t")
unitrootTable(trend="c",statistic="t")
unitrootTable(trend="nc",statistic="t")










