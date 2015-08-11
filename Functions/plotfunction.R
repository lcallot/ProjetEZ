
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
    time<-seq(as.Date("2008/1/1"), as.Date("2016/12/1"), by = "quarter")
    df$time<-time
    D=melt(df, id='time')
    a<-ggplot(D,aes(time,value, group=variable, color=variable))+geom_line()
    
  } else {
    
    df<-data.frame(e)
    time<-seq(as.Date("2007/1/1"), as.Date("2016/12/1"), by = "quarter")
    df$time<-time
    D=melt(df, id='time')
    a<-ggplot(D,aes(time,value, group=variable, color=variable))+geom_line()
  }
  
  return(a)
}