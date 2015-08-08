bootlassovar<-function(data,lag,iter,adap,TREND){
  
  lv<-lassovar(data,lags=lag,adaptive=adap,trend=TREND)
  res<-residuals(lv)
  pred<-lv$y-res
  coef<-as.matrix(lv$coefficients[2:(lag*dim(data)[2]+1),],lag*dim(data)[2],dim(data)[2])
  intercept<-matrix(lv$coefficients[1,],1, dim(data)[2])
  
  if (TREND==TRUE){
    trend<-as.matrix(lv$coefficients[lag*dim(data)[2]+2,],1,dim(data)[2])
  } else {
    trend<-matrix(0,1,dim(data)[2])
  }
  
  residu<-NULL
  for (l in 1:dim(res)[2]){
    residu<-cbind(residu,res[,l]-mean(res[,l]))
  }
  
  bootcoef<-list()
  for (i in 1:iter){
    estar=matrix(0,dim(data)[1],dim(data)[2])
    ystar=matrix(0,dim(data)[1],dim(data)[2])
    for (j in 1:dim(data)[1]){
      estar[j,]<-residu[sample(seq(from=1,to=dim(residu)[1],by=1),1,replace = T),]
    }
    
    for (m in 1:lag){
      ystar[m,]<-as.matrix(data[m,])+estar[m,]
    }
    
    for (t in (lag+1):dim(data)[1]){
      
      for (l in 1:lag){
        ystar[t,]<-ystar[t,] + ystar[t-l,]%*%coef[(1+(l-1)*dim(data)[2]):(dim(data)[2]*l),]
      } 
      ystar[t,]<-intercept + ystar[t,]
      ystar[t,]<- ystar[t,] + estar[t,]
      ystar[t,]<- ystar[t,] + trend*t
    }
    
    lvboot<-lassovar(ystar,lags=lag,adaptive=adap,trend=TREND)
    bootcoef[[i]]<-lvboot$coef
  }
  return(bootcoef)
}  
