load("Agrege/Base/vardata.Rdata")

#require('reshape2')
#require('ggplot2')
library('lassovar')
#library('dplyr')
#library('urca')
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
Ddf <- tail(df,-1) - head(df,-1)

df<-df[81:176,]


x<-rep(0,92)
for (i in 1:92){
  x[i]<-df$HICPSA[i+4]-df$HICPSA[i]
}


sub<- df[5:96,]

sub$date<-NULL
sub$HICPSA<-x
#Dsub <- tail(sub,-1) - head(sub,-1)

