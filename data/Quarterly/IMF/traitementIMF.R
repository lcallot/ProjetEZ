                      ## IMF Database (quarterly) ## 


# GDP deflator
Gdp_deflator <- read.csv("data/Quarterly/IMF/Gdp_deflator.csv", sep=";", dec=",")
deflator<-Gdp_deflator[,1:16]

    # log transformation
deflator[,-1]<-log(deflator[,-1])




#PPI without Luxembourg
PPI <- read.csv("data/Quarterly/IMF/PPI.csv", sep=";", dec=",")
PPI$Time<-NULL

    # log transformation
PPI<-log(PPI)



#Monetary Agregate
Monetary <- read.csv("data/Quarterly/IMF/M1_M3quarterly.csv")
colnames(Monetary)<-c("time","M1_EZ","M3_EZ")
Monetary$time<-NULL

    # transormation
Monetary<-Monetary/1000000
Monetary<-log(Monetary)




# Money rate EZ
rates <- read.csv("~/Documents/Stage VU/ProjetEZ/data/Quarterly/IMF/rates.csv")
MONRATE_EZ<-rates[,2]
MONRATE_EZ<-rbind(as.matrix(rep(NA,16)),as.matrix(MONRATE_EZ))
MONRATE_EZ<-data.frame(MONRATE_EZ)

    #  /100 transformation
MONRATE_EZ<-MONRATE_EZ/100



# Industrial production, oil, CB asset, taux directeurs, LIB3M
load("data/Quarterly/IMF/imfq.RData")





# Data from Q1 1990
dfimfq<-data.frame(deflator,PPI,Monetary,MONRATE_EZ,subset)
save(dfimfq, file="data/Quarterly/IMF/dfimfq.RData")


# Differentiation of the dataset
dfimfq$Time<-NULL
difdfimfq <- tail(dfimfq[,-1],-1) - head(dfimfq[,-1],-1)


#save differentiated database from Q2 1990
save(difdfimfq, file="data/Quarterly/IMF/difdfimfq.RData")


