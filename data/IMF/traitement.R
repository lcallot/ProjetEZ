#Monetary Agregate
MonetaryAgreg <- read.csv("data/IMF/M1_M3.csv")
colnames(MonetaryAgreg)<-c("time","M1_ROW","M3_ROW")

#Oil price
oilprice <- read.csv("data/IMF/oilprice.csv")
oilprice[,3] <- NULL
colnames(oilprice)<-c("time","OIL_ROW")


