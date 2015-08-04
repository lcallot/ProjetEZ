
forecast<-function(data,lag,preforecast,horizon,adap,trend){
  fore<-matrix(0,nrow=dim(data)[2],ncol=horizon+preforecast)
  fore[,1:(preforecast)]<-t(data[(dim(data)[1]-preforecast+1):dim(data)[1],])
  lv<-lassovar(dat=data,lags=lag,adaptive=adap,trend=trend)
  intercept<-as.matrix(lv$coefficients[1,],dim(data)[2],1)
  if (trend==TRUE){
    trend<-as.matrix(lv$coefficients[lag*dim(data)[2]+2,],dim(data)[2],1)
  } else {
    trend<-matrix(0,dim(data)[2],1)
  }
  coef<-as.matrix(lv$coefficients[2:(lag*dim(data)[2]+1),],lag*dim(data)[2],dim(data)[2])
  
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