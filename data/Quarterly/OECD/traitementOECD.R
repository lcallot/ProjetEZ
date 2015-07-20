# Interest rates
load("data/Quarterly/OECD/oecd.RData")
dfoecd<-df



data<-data.frame(dfecb,dfimf,dfoecd)
setwd("~/Documents/Stage VU/ProjetEZ/data/Quarterly")
save(data, file="vardata.RData")
