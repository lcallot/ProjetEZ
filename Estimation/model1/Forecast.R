require(lassovar)
require(ggplot2)
require(reshape2)
require(urca)
require(MSBVAR)



new<-function(data,lag,horizon,preforecast,adap){
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