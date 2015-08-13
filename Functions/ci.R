ci<-function(data,lag,adap,post,h){
  lv<-lassovar(data,lags=lag,adaptive=adap, post=TRUE ,ncores=TRUE)
  
  if (post=="yes"){
    cons<-lv$post[1,]
    coef<-matrix(lv$post[2:(lag*dim(df)[2]+1),],lag*dim(df)[2])
  } else{ 
    cons<-lv$coef[1,]
    coef<-matrix(lv$coef[2:(lag*dim(df)[2]+1),],lag*dim(df)[2])
  }
  
  sigma<-cov(lv$y-lv$x%*%coef)
  psy<-vector("list",h)
  psy[[1]]<-diag(x = 1, dim(df)[2], dim(df)[2])
  MSE<-vector("list",h)
  MSE[[1]]<-psy[[1]]%*%sigma%*%t(psy[[1]])
  for (s in 2:h){
    for (j in 1:(min(lag-1,s-1))){
      if (is.null(psy[[s]])){
        psy[[s]]<-matrix(0,dim(df)[2],dim(df)[2])
      } else {}
      psy[[s]]<-psy[[s]]+psy[[s-j]]%*%coef[(dim(df)[2]*(j-1)+1):(j*dim(df)[2]),]
    }
    for (l in 1:s){
      if (is.null(MSE[[s]])){
        MSE[[s]]<-matrix(0,dim(df)[2],dim(df)[2])
      } else {}
      MSE[[s]]<-MSE[[s]]+psy[[l]]%*%sigma%*%t(psy[[l]])  
    }
  }
  return(MSE)
}