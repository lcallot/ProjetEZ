


RW<-function(t,x,mean,var){
  y<-matrix(0,1,t)
  y[,1]<-x
  for(i in 2:t){
    y[,i] <- y[,i-1] + mvrnorm(n = 1, mu= mean , Sigma= var)
  }
  return(t(y))
}