#Monetary Agregate
MonetaryAgreg <- read.csv("data/IMF/M1_M3.csv")
colnames(MonetaryAgreg)<-c("time","M1_ROW","M3_ROW")

#Oil price
oilprice <- read.csv("data/IMF/oilprice.csv")
oilprice[,3] <- NULL
colnames(oilprice)<-c("time","OIL_ROW")

#LIBOR 3M US
LIBOR3M_US <- read.csv("data/IMF/LIBOR3M_US.csv")


# Taux directeur et Bilan ZE and US
tauxdirecteuretbilanBC <- read.csv("data/IMF/tauxdirecteuretbilanBC.csv")
colnames(tauxdirecteuretbilanBC) <- c("time","CBASSET_ZE","POLRATE_ZE","CBASSET_US","POLRATE_US")


# Industrial Production
Prodindus <- read.csv("data/IMF/Prodindus.csv", sep=";", dec=",")
colnames(Prodindus) <- c("time","PINDUS_AUS","PINDUS_BEL","PINDUS_DEN","PINDUS_FIN","PINDUS_FRA",
                         "PINDUS_GER","PINDUS_GRE","PINDUS_IRL","PINDUS_ITA","PINDUS_LUX",
                         "PINDUS_NET","PINDUS_POR","PINDUS_SPA","PINDUS_SWE","PINDUS_UKI")




