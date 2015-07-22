require(lassovar)
require(ggplot2)
require(reshape2)
require(urca)
require(MSBVAR)
library(MASS)

# I load differentiated variables from Q2 1990 
load("data/difvardatam.RData")

# I keep HICP variable only
dataM1<-difvardatam[,c("HICP_AUS","HICP_BEL", "HICP_GER","HICP_DEN",
                       "HICP_FRA","HICP_UKI", "HICP_SPA","HICP_GRE",
                       "HICP_FIN","HICP_IRL","HICP_ITA","HICP_LUX",
                       "HICP_POR","HICP_SWE","HICP_NET","M1_ZE")]

dataM3<-difvardatam[,c("HICP_AUS","HICP_BEL", "HICP_GER","HICP_DEN",
                       "HICP_FRA","HICP_UKI", "HICP_SPA","HICP_GRE",
                       "HICP_FIN","HICP_IRL","HICP_ITA","HICP_LUX",
                       "HICP_POR","HICP_SWE","HICP_NET","M3_ZE")]

# I keep variable from Jan 1996

dataM1<-dataM1[73:300,]
dataM3<-dataM3[73:300,]
dataM1ts<-ts(dataM1,start=1996,frequency=12)



## Choix du nb de lag
lagchoice<-function(data,lagmax){
  AIC<-NULL
  HQ<-NULL
  SC<-NULL
  for (i in 1:lagmax){
    lv<-lassovar(data, lags=i)
    cons<-lv$coefficients[1,]
    coef<-lv$coefficients[-1,]
    res<-lv$y-matrix(rep(1,dim(lv$y)[1]))%*%t(cons)-lv$x%*%coef
    Sigma<-((dim(lv$y)[1])^-1)*(t(as.matrix(res))%*%as.matrix(res))
    det<-det(Sigma)
    AIC[i]<-log(det)+2*i*dim(data)[2]^2/dim(subset)[1]
    HQ[i]<-log(det)+2*log(log(dim(data)[1]))*i*dim(data)[2]^2/dim(data)[1]
    SC[i]<-log(det)+log(dim(data)[1])*i*dim(data)[2]^2/dim(data)[1]
  }
  return(rbind(AIC,HQ,SC))
}
lagchoice(dataM1,lagmax=10)




# Forecast

fun<-function(data,lag,preforecast,horizon,adap){
  fore<-matrix(0,nrow=dim(data)[2],ncol=horizon+preforecast)
  fore[,1:(preforecast)]<-t(data[(dim(data)[1]-preforecast+1):dim(data)[1],])
  lv<-lassovar(dat=data,lags=lag,adaptive=adap,trend=TRUE)
  intercept<-as.matrix(lv$coefficients[1,],dim(data)[2],1)
  coef<-as.matrix(lv$coefficients[2:(lag*dim(data)[2]+1),],lag*dim(data)[2],dim(data)[2])
  trend<-as.matrix(lv$coefficients[lag*dim(data)[2]+2,],dim(data)[2],1)
  
  for (i in (preforecast+1):(horizon+preforecast)){
    M<-NULL
    for (l in 1:lag){
      M<-c(fore[,(i-l)],M)
    }
    fore[,i]<-t(M%*%coef)+intercept+trend*(i+dim(data)[1]-(preforecast))
  }
  rownames(fore)<-names(data)
  return(t(fore))
}
forecast<-fun(dataM1,5,12,36,"none")
forecast<-data.frame(forecast)
var1<-forecast[,1:5]
var2<-forecast[,6:10]
var3<-forecast[,11:16]
time<-seq(as.Date("2013/01/01"), as.Date("2016/12/31"), by = "month")
var1$time<-time
var2$time<-time
var3$time<-time
mvar1 <- melt(var1,  id = 'time', variable.name = 'series')
mvar2 <- melt(var2,  id = 'time', variable.name = 'series')
mvar3 <- melt(var3,  id = 'time', variable.name = 'series')
ggplot(mvar1, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")
ggplot(mvar2, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")
ggplot(mvar3, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")





# Condtional forecast

rw<-function(t,x){
  y<-matrix(0,dim(x)[2],t)
  y[,1]<-as.matrix(exo[dim(x)[1],])
  if(dim(exo)[2]==1) {
    for(i in 2:t){
      y[,i] <- y[,i-1] + matrix(mvrnorm(n = 1, mu=rep(0,dim(x)[2]) , Sigma=var(x)),dim(x)[2],1)
    }
  }
  else {
    for(i in 2:t){
      y[,i] <- y[,i-1] + matrix(mvrnorm(n = 1, mu=rep(0,dim(x)[2]) , Sigma=diag(diag(var(x)))),dim(x)[2],1)
    }
  }
  return(y[,-1])
}

conditional<-function(exogen,data,lag,preforecast,horizon,adap){
  all=data.frame(exogen,data)
  fore<-matrix(0,nrow=dim(all)[2],ncol=horizon+preforecast)
  fore[,1:(preforecast)]<-t(all[(dim(all)[1]-preforecast+1):dim(all)[1],])
  fore[1:dim(exogen)[2],-c(1:(preforecast))]<-rw(horizon+1,exogen)
  lv<-lassovar(dat=all,lags=lag,adaptive=adap)
  intercept<-as.matrix(lv$coefficients[1,],dim(all)[2],1)
  coef<-as.matrix(lv$coefficients[2:(lag*dim(all)[2]+1),],lag*dim(all)[2],dim(all)[2])
  for (i in (preforecast+1):(horizon+preforecast)){
    M<-NULL
    for (l in 1:lag){
      M<-c(fore[,(i-l)],M)
    }
    fore[-c(1:dim(exogen)[2]),i]<-(t(M%*%coef)+intercept)[-c(1:dim(exogen)[2]),]
  }
  
  rownames(fore)<-names(all)
  return(t(fore))
}

exo<-data.frame(dataM1[,16])
end<-dataM1[,-16]



forecast<-conditional(exo,end,2,12,36,"none")
forecast<-data.frame(forecast)
var1<-forecast[,1:5]
var2<-forecast[,6:10]
var3<-forecast[,11:16]
time<-seq(as.Date("2013/01/01"), as.Date("2016/12/31"), by = "month")
var1$time<-time
var2$time<-time
var3$time<-time
mvar1 <- melt(var1,  id = 'time', variable.name = 'series')
mvar2 <- melt(var2,  id = 'time', variable.name = 'series')
mvar3 <- melt(var3,  id = 'time', variable.name = 'series')
ggplot(mvar1, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")
ggplot(mvar2, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")
ggplot(mvar3, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")


iter<-50
preforecast<-12
horizon<-36

exo<-data.frame(dataM1[,16])
end<-dataM1[,-16]

HICPpred<-matrix(0,horizon+preforecast,iter)
for (i in 1:iter){
  HICPpred[,i]<-matrix(conditional(exo,end,4,preforecast,horizon,"none")[,"HICP_FRA"])
}
HICPpred
HICPpred<-data.frame(HICPpred)
HICPpred$time<-seq(as.Date("2013/01/01"), as.Date("2016/12/31"), by = "month")
var <- melt(HICPpred,  id = 'time', variable.name = 'series')
ggplot(var, aes(time,value, col=series)) + geom_point() + geom_line(linetype="solid") 







