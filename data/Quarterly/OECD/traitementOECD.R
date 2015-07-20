# Interest rates
load("data/Quarterly/OECD/oecd.RData")
dfOECD<-df



data<-data.frame(dfecb,dfIMF,dfOECD)
setwd("~/Documents/Stage VU/ProjetEZ/data/Quarterly")
save(data, file="vardata.RData")
