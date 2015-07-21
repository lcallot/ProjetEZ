HICP<-HICP[37:336,]
MONAGR<-MonetaryAgreg[1:300,]
MONAGR$time<-NULL
rownames(HICP)<-NULL
data1<-data.frame(HICP,MONAGR)

