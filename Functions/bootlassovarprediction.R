bootlassovar.prediction<-function(data,bootcoefficient,lag,preforecast,horizon){
  
  pre<-list()
  for (j in 1:length(bootcoef)){
    intercept<-as.matrix(bootcoef[[j]][1,])
    coef<-as.matrix(bootcoef[[j]][2:(lag*dim(bootcoef[[j]])[2]+1),],
                    lag*dim(bootcoef[[j]])[2],dim(bootcoef[[j]])[2])
    
    fore<-matrix(0,nrow=dim(data)[2],ncol=horizon+preforecast)
    fore[,1:(preforecast)]<-t(data[(dim(data)[1]-preforecast+1):dim(data)[1],])
    
    for (i in (preforecast+1):(horizon+preforecast)){
      M<-NULL
      for (l in 1:lag){
        M<-c(fore[,(i-l)],M)
      }
      fore[,i]<-t(M%*%coef)+intercept
    }
    rownames(fore)<-names(data)
    pre[[j]]<-t(fore)
    
  }
  return(pre)
}
