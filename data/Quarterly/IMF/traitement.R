# GDP deflator
Gdp_deflator <- read.csv("data/Quarterly/IMF/Gdp_deflator.csv", sep=";", dec=",")

deflator<-Gdp_deflator[,1:16]


#PPI without Luxembourg
PPI <- read.csv("data/Quarterly/IMF/PPI.csv", sep=";", dec=",")

#Monetary Agregate
Monetary <- read.csv("data/Quarterly/IMF/M1_M3quarterly.csv")
colnames(Monetary)<-c("time","M1_ZE","M3_ZE")

# Industrial production, 
load("subset.RData")

PPI$Time<-NULL
Monetary$time<-NULL
df<-data.frame(deflator,PPI,Monetary)


