load("data/vardatam.RData")

require('reshape2')
require('ggplot2')
library('lassovar')
#library('dplyr')
library('urca')
#library('stats')

source("Functions/forecastfunction.R")
source("Functions/bootlassovar.R")
source("Functions/bootlassovarprediction.R")

df<- vardatam
df<- df[109:288,]
#df<-var
# data from Q4 1989 to 2013 Q4 (80:176)
#df<-sub1[80:176,]
df$time<-NULL

#Ddf <- tail(df[,-1],-1) - head(df[,-1],-1)

dput(names(df))

#df2<-df[,c("HICP_AUS", "HICP_BEL",  "HICP_GER",  "HICP_DEN",  "HICP_SPA",  
 #          "HICP_FIN",  "HICP_FRA", "HICP_UKI",  "HICP_GRE",  "HICP_IRL",  
  #         "HICP_ITA",  "HICP_NET", "HICP_POR",  "HICP_SWE", 
           
  #      "UNPLOY_AUS", "UNPLOY_BEL", "UNPLOY_GER", "UNPLOY_DEN", "UNPLOY_SPA", 
   #        "UNPLOY_FIN", "UNPLOY_FRA", "UNPLOY_UKI",  "UNPLOY_IRL", 
    #       "UNPLOY_ITA", "UNPLOY_NET", "UNPLOY_POR", "UNPLOY_SWE", 
     #      "exchrate", "M1_ZE", "M3_ZE", "OIL_ROW", "LIB3_US",  
   #        "POLRATE_ZE",  "POLRATE_US", "PINDUS_AUS", "PINDUS_BEL", 
    #       "PINDUS_DEN", "PINDUS_FIN", "PINDUS_FRA", "PINDUS_GER", "PINDUS_GRE", 
    #      "PINDUS_IRL", "PINDUS_ITA",  "PINDUS_NET", "PINDUS_POR", 
    #      "PINDUS_SPA", "PINDUS_SWE", "PINDUS_UKI", "STR_AUS", "STR_BEL", 
     #      "STR_DEN", "STR_FIN", "STR_FRA", "STR_GER", "STR_GRE", "STR_IRL", 
      #     "STR_ITA",  "STR_NET", "STR_POR", "STR_SPA", "STR_SWE", 
       #    "STR_UKI", "LTR_AUS", "LTR_BEL", "LTR_DEN", "LTR_FIN", "LTR_FRA", 
        #   "LTR_GER", "LTR_GRE", "LTR_IRL", "LTR_ITA",  "LTR_NET", 
        #  "LTR_POR", "LTR_SPA", "LTR_SWE", "LTR_UKI")]

df2<-df[,c("HICP_AUS", "HICP_BEL",  "HICP_GER",  "HICP_DEN",  "HICP_SPA",  
          "HICP_FIN",  "HICP_FRA", "HICP_UKI",  "HICP_GRE",  "HICP_IRL",  
         "HICP_ITA",  "HICP_NET", "HICP_POR",  "HICP_SWE",  "UNPLOY_FRA", 
       "exchrate",  "M3_ZE", "OIL_ROW", "LIB3_US",  "POLRATE_ZE",
       "POLRATE_US", "PINDUS_FRA", "STR_FRA", "LTR_FRA")]



foreca<-forecast(df2,4,12,16,"lasso",TRUE)
foreca<-data.frame(foreca)
var1<-foreca[,1:4]
time<-seq(as.Date("2011/1/1"), as.Date("2017/12/1"), by = "quarter")
var1$time<-time
mvar1 <- melt(var1,  id = 'time', variable.name = 'series')
ggplot(mvar1, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")



# bootlassovar
iter=1
adap="lasso"
lag=4
preforecast=28
horizon=16
TREND=TRUE
data=df2


bootcoef<-bootlassovar(df2,lag,iter,adap,TRUE)
Q<-bootlassovar.prediction(df2,bootcoef,lag,preforecast,horizon)

name="HICP_EZ"
liste=Q
lenght=preforecast+horizon
freq=4

tryfun<-function(liste,name,lenght,iter,yoy,freq){
  
  e<-matrix(NA,lenght,iter)
  for (i in 1:iter){
    e[,i]<-liste[[i]][,name]
  }
  
  if (yoy=="yes"){
    f<-matrix(NA,lenght-freq,iter)
    for (j in 1:(lenght-freq)){
      f[j,]<-e[j+freq,]-e[j,]
    }
    
    df<-data.frame(f[-(1:freq),])
    time<-seq(as.Date("2009/1/1"), as.Date("2017/12/1"), by = "quarter")
    df$time<-time
    D=melt(df, id='time')
    a<-ggplot(D,aes(time,value, group=variable, color=variable))+geom_line()
    
  } else {
    
    df<-data.frame(e)
    time<-seq(as.Date("2008/1/1"), as.Date("2017/12/1"), by = "quarter")
    df$time<-time
    D=melt(df, id='time')
    a<-ggplot(D,aes(time,value, group=variable, color=variable))+geom_line()
  }
  
  return(a)
}
tryfun(Q,"HICP_EZ",preforecast+horizon,iter,"yes",4)


