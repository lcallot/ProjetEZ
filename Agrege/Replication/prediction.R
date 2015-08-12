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



# bootlassovar
iter=10
adap="ridge"
lag=12
preforecast=28
horizon=12
TREND="no"



bootcoef<-bootlassovar(sub[1:80,],lag,iter,adap,FALSE)
Q1<-bootlassovar.prediction(sub[1:80,],bootcoef,lag,preforecast,horizon,"no")
#save(Q1,file="Agrege/Replication/Q1.Rdata")

name="HICPSA"
plotfunction(Q1,name,preforecast+horizon,iter,"no",4)
plot(x=seq(as.Date("2004/01/1"), as.Date("2013/12/1"), by = "quarter"),y=sub$HICPSA[53:92],type="l")




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
    time<-seq(as.Date("2005/1/1"), as.Date("2013/12/1"), by = "quarter")
    df$time<-time
    D=melt(df, id='time')
    a<-ggplot(D,aes(time,value, group=variable, color=variable))+geom_line()
    
  } else {
    
    df<-data.frame(e)
    time<-seq(as.Date("2004/1/1"), as.Date("2013/12/1"), by = "quarter")
    df$time<-time
    D=melt(df, id='time')
    a<-ggplot(D,aes(time,value, group=variable, color=variable))+geom_line()
  }
  
  return(a)
}
