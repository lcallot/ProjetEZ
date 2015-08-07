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


foreca<-forecast(sub[,-1],1,12,16,"none",FALSE)
foreca<-data.frame(foreca)
var1<-foreca[,1:4]
time<-seq(as.Date("2011/1/1"), as.Date("2017/12/1"), by = "quarter")
var1$time<-time
mvar1 <- melt(var1,  id = 'time', variable.name = 'series')
ggplot(mvar1, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")


# bootlassovar
bootcoef<-bootlassovar(sub[,-1],4,10,"lasso")




lv<-lassovar(sub[,-1], lags=4, adaptive="lasso",post = TRUE)
res<-residuals(lv)
pred<-lv$y-res
coef<-as.matrix(lv$coefficients[2:(lag*dim(data)[2]+1),],lag*dim(data)[2],dim(data)[2])
intercept<-matrix(lv$coefficients[1,],1, dim(data)[2])





