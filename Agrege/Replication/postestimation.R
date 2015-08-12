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

#Dsub <- tail(sub,-1) - head(sub,-1)
df$date<-NULL

lag=4
lv<-lassovar(df,lags=lag,adaptive="none", post=TRUE ,ncores=TRUE)

dim(lv$post)

cons<-lv$post[1,]
coef<-matrix(lv$post[2:(lag*dim(df)[2]+1),],lag*dim(df)[2])

h=12

psy<-vector("list",h)
psy[[1]]<-diag(x = 1, dim(df)[2], dim(df)[2])
psy[[2]]<-psy[[1]]%*%coef[1:(dim(df)[2]),]
psy[[3]]<-psy[[2]]%*%coef[1:(dim(df)[2]),] + psy[[1]]%*%coef[(dim(df)[2]+1):(2*dim(df)[2]),]

for (i in 0:(h-1)){
  

   
}

