
forecastlahiri<-function(data,lag,preforecast,horizon,adap,TREND,name,alpha){
  fore<-matrix(0,nrow=dim(data)[2],ncol=horizon+preforecast)
  fore[,1:(preforecast)]<-t(data[(dim(data)[1]-preforecast+1):dim(data)[1],])
 
  foreca<-matrix(NA,nrow=3,ncol=horizon+preforecast)
  foreca[1,1:(preforecast)]<-t(data[(dim(data)[1]-preforecast+1):dim(data)[1],name])
  
  lv<-lassovar(dat=data,lags=lag,adaptive=adap,trend=TREND)
  intercept<-as.matrix(lv$coefficients[1,],dim(data)[2],1)
  
  a<-match("HICPSA",names(df2))

  if (TREND==TRUE){
    trend<-as.matrix(lv$coefficients[lag*dim(data)[2]+2,],dim(data)[2],1)
  } else {
    trend<-matrix(0,dim(data)[2],1)
  }
  coef<-as.matrix(lv$coefficients[2:(lag*dim(data)[2]+1),],lag*dim(data)[2],dim(data)[2])
  
  #residu 
  q1<-quantile((lv$y-(lv$x)%*%(lv$coef[2:(lag*dim(data)[2]+1),]))[,name], 1-alpha/2 )
  q2<-quantile((lv$y-(lv$x)%*%(lv$coef[2:(lag*dim(data)[2]+1),]))[,name], alpha/2) 

  
  
  for (i in (preforecast+1):(horizon+preforecast)){
    M<-NULL
    for (l in 1:lag){
      M<-c(fore[,(i-l)],M)
    }
    fore[,i]<-t(M%*%coef)+intercept+trend*(i+dim(data)[1]-(preforecast))

    foreca[1,i]<-matrix(t(M%*%coef)+intercept+trend*(i+dim(data)[1]-(preforecast)))[a]
    foreca[2,i]<-matrix(t(M%*%coef)+intercept+trend*(i+dim(data)[1]-(preforecast)))[a]+q1
    foreca[3,i]<-matrix(t(M%*%coef)+intercept+trend*(i+dim(data)[1]-(preforecast)))[a]+q2
  }
  rownames(foreca)<-c(name,"ci1","ci2")
  return(t(foreca))
}