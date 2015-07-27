
load("Agrege/Base/vardata.Rdata")
# data from Q1 1990 to 2013 Q4 (81:176)
sub<-var[81:176,]
sub<- sub[,c('date','M1','M3','STN','LTN','YED','YER','URX','POILU','LHO', 'LFI')]


lm<-lm(sub$YED ~ sub$M3 + sub$YER)
pred<-as.matrix(lm$fitted.value)
coef<-lm$coef
e<-lm$res
etild<-e-mean(e)

iter<-10000
t=matrix(NA,3,iter)
estar=matrix(NA,length(etild),iter)
ystar=matrix(NA,length(etild),iter)

for (i in 1:iter){
  
  estar[,i]<-as.matrix(sample(etild,length(etild),replace = T))
  ystar[,i]<-pred+estar[,i]
  
  boot<-lm(ystar[,i]~sub$M3 + sub$YER)
  bootcoef<-boot$coef
  t[,i]<-as.matrix(sqrt(length(ystar[,1]))*(bootcoef-coef))
  r[,i]<-
}

plot(density(t[1,]))







