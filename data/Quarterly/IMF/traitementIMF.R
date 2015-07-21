# GDP deflator
Gdp_deflator <- read.csv("data/Quarterly/IMF/Gdp_deflator.csv", sep=";", dec=",")

deflator<-Gdp_deflator[,1:16]


#PPI without Luxembourg
PPI <- read.csv("data/Quarterly/IMF/PPI.csv", sep=";", dec=",")
PPI$Time<-NULL

#Monetary Agregate
Monetary <- read.csv("data/Quarterly/IMF/M1_M3quarterly.csv")
colnames(Monetary)<-c("time","M1_EZ","M3_EZ")
Monetary$time<-NULL

# Money rate EZ
rates <- read.csv("~/Documents/Stage VU/ProjetEZ/data/Quarterly/IMF/rates.csv")
MONRATE_EZ<-rates[,2]
MONRATE_EZ<-rbind(as.matrix(rep(NA,16)),as.matrix(MONRATE_EZ))
MONRATE_EZ<-data.frame(MONRATE_EZ)


# Industrial production, oil, CB asset, taux directeurs
load("data/Quarterly/IMF/subset.RData")






dfimf<-data.frame(deflator,PPI,Monetary,MONRATE_EZ,subset)
dfimf$Time<-NULL
