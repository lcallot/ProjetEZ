                #### MONTE CARLO STUDY ####


# Generate the statistics

mcfunction<-function(x,p,nonzero,beta,n,boot,alpha,type){
  allzero<-TRUE
  while(allzero==TRUE){ 
    #1 : generate the data
    if (type=="iid1"){
      df<-iid1(p,beta,n)
    } else {
      if (type=="iid5"){
        df<-iid5(p,beta,beta,beta,-beta,-beta,n)
      } else {
        if (type=="iid10"){
          df<-iid10(p,beta,beta,beta,beta,beta,-beta,-beta,-beta,-beta,-beta,n)
        } else {
          if (type=="AR1"){
            df<-depAR1(p,beta,n)
          } else {
            df<-NULL
          } 
        }
      }
    }
    
    # estimate ALASSO
    allzero<-(sum((lasso(y~.,df)$coef[-1])!=0)==0)
  }
    # Calculate the statistics
    result<-lahiriboot2(df,boot,alpha,nonzero,c(beta,beta,beta,-beta,-beta))
  
  return(result)
}


# Replicate and mean of the statistics
MC<-function(iter,p,nonzero,beta,n,boot,alpha,type){
  #a<-replicate(iter, mcfunction(p,nonzero,beta,n,boot,alpha,type))
  #res<-list()
  #for (i in 1:5){
  #  res[[i]]<-unlist(a[i,]) 
  #}
  
  #b<-lapply(1:iter, mcfunction,p,nonzero,beta,n,boot,alpha,type)
  b<-mclapply(1:iter, mcfunction,p,nonzero,beta,n,boot,alpha,type,mc.cores = 2)
  res <- matrix(unlist(b),nrow=5)

  
  return(res)
}