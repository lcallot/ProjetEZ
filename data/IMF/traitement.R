#Monetary Agregate
MonetaryAgreg <- read.csv("data/IMF/M1_M3.csv")
colnames(MonetaryAgreg)<-c("time","M1_ZE","M3_ZE")

#Oil price
oilprice <- read.csv("data/IMF/oilprice.csv")
oilprice[,3] <- NULL
colnames(oilprice)<-c("time","OIL_ROW")

#LIBOR 3M US
LIBOR3M_US <- read.csv("data/IMF/LIBOR3M_US.csv")
colnames(LIBOR3M_US)<-c("time","LIB3_US")

# Taux directeur et Bilan ZE and US
tauxdirecteuretbilanBC <- read.csv("data/IMF/tauxdirecteuretbilanBC.csv")
colnames(tauxdirecteuretbilanBC) <- c("time","CBASSET_ZE","POLRATE_ZE","CBASSET_US","POLRATE_US")


# Industrial Production
Prodindus <- read.csv("data/IMF/Prodindus.csv", sep=";", dec=",")
colnames(Prodindus) <- c("time","PINDUS_AUS","PINDUS_BEL","PINDUS_DEN","PINDUS_FIN","PINDUS_FRA",
                         "PINDUS_GER","PINDUS_GRE","PINDUS_IRL","PINDUS_ITA","PINDUS_LUX",
                         "PINDUS_NET","PINDUS_POR","PINDUS_SPA","PINDUS_SWE","PINDUS_UKI")



df<-data.frame(MonetaryAgreg,oilprice[-305,],LIBOR3M_US[-305,],tauxdirecteuretbilanBC[-305,],Prodindus)
df2<-df[-304,]
df2$time.1<-NULL
df2$time.2<-NULL
df2$time.3<-NULL
df2$time.4<-NULL


df2$time<-NULL
dfts <- ts(df2, ,start=c(1990,1),frequency=12)
dfq <- aggregate(dfts, FUN=sum, nfrequency=4)/3
df<-data.frame(dfq)

subset<-df[,-(1:2)]
setwd("~/Documents/Stage VU/ProjetEZ/data/Quarterly/IMF")
save(subset, file="subset.RData")
