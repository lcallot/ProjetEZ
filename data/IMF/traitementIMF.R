                        ## IMF (IFS) Database ##


## Monetary Agregate
MonetaryAgreg <- read.csv("data/IMF/M1_M3.csv")
colnames(MonetaryAgreg)<-c("time","M1_ZE","M3_ZE")

    # Log transformation

MonetaryAgreg[,-1]<-log(MonetaryAgreg[,-1])


## Oil price
oilprice <- read.csv("data/IMF/oilprice.csv")
oilprice[,3] <- NULL
colnames(oilprice)<-c("time","OIL_ROW")

    # Log transformation

oilprice[,-1]<-log(oilprice[,-1])



## LIBOR 3M US
LIBOR3M_US <- read.csv("data/IMF/LIBOR3M_US.csv")
colnames(LIBOR3M_US)<-c("time","LIB3_US")

    # /100 transformation

LIBOR3M_US[,-1]<-LIBOR3M_US[,-1]/100


## Taux directeur et Bilan ZE and US
tauxdirecteuretbilanBC <- read.csv("data/IMF/tauxdirecteuretbilanBC.csv")
colnames(tauxdirecteuretbilanBC) <- c("time","CBASSET_ZE","POLRATE_ZE","CBASSET_US","POLRATE_US")

    # /100 transformation for policy rate 
    # log transformation for Central Bank asset

tauxdirecteuretbilanBC[,2]<-log(tauxdirecteuretbilanBC[,2])
tauxdirecteuretbilanBC[,4]<-log(tauxdirecteuretbilanBC[,4])
tauxdirecteuretbilanBC[,3]<-tauxdirecteuretbilanBC[,3]/100
tauxdirecteuretbilanBC[,5]<-tauxdirecteuretbilanBC[,5]/100


# Industrial Production
Prodindus <- read.csv("data/IMF/Prodindus.csv", sep=";", dec=",")
colnames(Prodindus) <- c("time","PINDUS_AUS","PINDUS_BEL","PINDUS_DEN","PINDUS_FIN","PINDUS_FRA",
                         "PINDUS_GER","PINDUS_GRE","PINDUS_IRL","PINDUS_ITA","PINDUS_LUX",
                         "PINDUS_NET","PINDUS_POR","PINDUS_SPA","PINDUS_SWE","PINDUS_UKI")

    #log transformation
Prodindus[,-1]<-log(Prodindus[,-1])



## Merge of IMF variable. Starting date : Jan 1990

df<-data.frame(MonetaryAgreg,oilprice[-305,],LIBOR3M_US[-305,],tauxdirecteuretbilanBC[-305,],Prodindus)
df2<-df[-304,]
df2$time.1<-NULL
df2$time.2<-NULL
df2$time.3<-NULL
df2$time.4<-NULL

imf<-df2

#data from Jan 1990 
save(imf, file="data/IMF/imf.RData")


# Differentiation of the database / starting date : Feb 1990
difdf2 <- tail(df2[,-1],-1) - head(df2[,-1],-1)
rownames(difdf2)<-NULL
save(difdf2, file="data/IMF/difimf.RData")





# In order to add variable not available in quarterly frequency
df2$time<-NULL
dfts <- ts(df2, ,start=c(1990,1),frequency=12)
dfq <- aggregate(dfts, FUN=sum, nfrequency=4)/3
df<-data.frame(dfq)

subset<-df[,-(1:2)]
save(subset, file="data/Quarterly/IMF/imfq.RData")
