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
source("Functions/plotfunction.R")

#dput(names(var))
df<-var[,c("date", "YER", "PCR", "GCR", "ITR", "XTR", "MTR", "YED",  
           "GCD", "ITD", "XTD", "MTD", "YFD", "YIN", "WIN", "TIN", 
           "YFN",  "HICPSA", "URX", "STN", "LTN","POILU", 
           "PCOMU", "YWR","WRN",  "EEN", "EXR",  "M1",
           "M3", "ESI", "LIB", "PPI", "DJES")]

df<- df[81:176,]
df$date<-NULL
Ddf <- tail(df,-1) - head(df,-1)



# bootlassovar
iter=100
adap="none"
lag=8
preforecast=28
horizon=16
TREND="no"

data=Ddf


bootcoef<-bootlassovar(Ddf,lag,iter,adap,FALSE)
Q<-bootlassovar.prediction(Ddf,bootcoef,lag,preforecast,horizon,"no")

save(Q,file="Agrege/Replication/Q.Rdata")

#name="HICPSA"
#plotfunction(Q,name,preforecast+horizon,iter,"no",4)


