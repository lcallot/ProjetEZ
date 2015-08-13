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
df<-var[,c("date", "YER", "PCR", "GCR", "ITR", "XTR", "MTR",  "ITD", "XTD", "MTD", "YFD", "YIN", "WIN", "TIN", 
           "YFN",  "HICPSA", "URX", "STN", "LTN","POILU", 
           "PCOMU", "YWR","WRN",  "EEN", "EXR",  "M1",
           "M3", "ESI", "LIB", "PPI", "DJES")]
Ddf <- tail(df,-1) - head(df,-1)

df<-df[81:176,]


x<-rep(0,92)
for (i in 1:92){
  x[i]<-df$HICPSA[i+4]-df$HICPSA[i]
}

<<<<<<< HEAD

=======
>>>>>>> 7f7f4b636ac849ab99294b21e09afcba7def60b4
#Dsub <- tail(sub,-1) - head(sub,-1)
df$date<-NULL

lag=4
lv<-lassovar(df,lags=lag,adaptive="none", post=TRUE ,ncores=TRUE)
lv$coef
dim(lv$post)


<<<<<<< HEAD









=======
cons<-lv$post[1,]
coef<-matrix(lv$post[2:(lag*dim(df)[2]+1),],lag*dim(df)[2])

h=12
lag=4

psy<-vector("list",h)
psy[[1]]<-diag(x = 1, dim(df)[2], dim(df)[2])
for (s in 2:h){

  for (j in 1:(min(lag-1,s-1))){
    if (is.null(psy[[s]])){
      psy[[s]]<-matrix(0,dim(df)[2],dim(df)[2])
    } else {}
    psy[[s]]<-psy[[s]]+psy[[s-j]]%*%coef[(dim(df)[2]*(j-1)+1):(j*dim(df)[2]),]
  }
}
>>>>>>> 7f7f4b636ac849ab99294b21e09afcba7def60b4

