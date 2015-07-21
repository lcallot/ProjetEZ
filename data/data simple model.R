HICP<-HICP[37:336,]
MONAGR<-MonetaryAgreg[1:300,]
MONAGR$time<-NULL
rownames(HICP)<-NULL
data1<-data.frame(HICP,MONAGR)

setwd("~/Documents/Stage VU/ProjetEZ/Estimation/model1")
save(data1, file="data1.RData")
