load("Agrege/Base/vardata.Rdata")

require('reshape2')
require('ggplot2')
library('lassovar')
library('dplyr')
library('urca')
library('stats')

source("Agrege/Replication/forecastfunction.R")
source("Agrege/Replication/bootlassovar.R")

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


# bootlassovar
bootlassovar(sub[,-1],4,10,"lasso")
