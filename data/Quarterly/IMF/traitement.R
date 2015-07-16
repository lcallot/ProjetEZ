# GDP deflator
Gdp_deflator <- read.csv("data/Quarterly/IMF/Gdp_deflator.csv", sep=";", dec=",")

deflator<-Gdp_deflator[,1:16]


#PPI without Luxembourg
PPI <- read.csv("data/Quarterly/IMF/PPI.csv", sep=";", dec=",")



PPI$Time<-NULL
df<-data.frame(deflator,PPI)
