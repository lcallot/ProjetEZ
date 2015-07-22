                                ## ECB Database ##


# Price index 

HICP_Finlande <- read.csv("data/ECB/HICP_Finlande.csv")
HICP_fin <- read.csv("data/ECB/HICP_fin.csv")
HICP_fin$X<-NULL
HICP<-data.frame(HICP_Finlande,HICP_fin)

nomenclature<-c("time","HICP_AUS","PFOOD_AUS","UPFOOD_AUS","INDGOOD_AUS","NRJ_AUS","SER_AUS",
                "HICP_BEL","PFOOD_BEL","UPFOOD_BEL","INDGOOD_BEL","NRJ_BEL","SER_BEL",
                "HICP_GER","PFOOD_GER","UPFOOD_GER","INDGOOD_GER","NRJ_GER","SER_GER",
                "HICP_DEN","PFOOD_DEN","UPFOOD_DEN","INDGOOD_DEN","NRJ_DEN","SER_DEN",
                "HICP_SPA","PFOOD_SPA","UPFOOD_SPA","INDGOOD_SPA","NRJ_SPA","SER_SPA",
                "HICP_FIN","PFOOD_FIN","UPFOOD_FIN","INDGOOD_FIN","NRJ_FIN","SER_FIN",
                "HICP_FRA","PFOOD_FRA","UPFOOD_FRA","INDGOOD_FRA","NRJ_FRA","SER_FRA",
                "HICP_UKI","PFOOD_UKI","UPFOOD_UKI","INDGOOD_UKI","NRJ_UKI","SER_UKI",
                "HICP_GRE","PFOOD_GRE","UPFOOD_GRE","INDGOOD_GRE","NRJ_GRE","SER_GRE",
                "HICP_IRL","PFOOD_IRL","UPFOOD_IRL","INDGOOD_IRL","NRJ_IRL","SER_IRL",
                "HICP_ITA","PFOOD_ITA","UPFOOD_ITA","INDGOOD_ITA","NRJ_ITA","SER_ITA",
                "HICP_LUX","PFOOD_LUX","UPFOOD_LUX","INDGOOD_LUX","NRJ_LUX","SER_LUX",
                "HICP_NET","PFOOD_NET","UPFOOD_NET","INDGOOD_NET","NRJ_NET","SER_NET",
                "HICP_POR","PFOOD_POR","UPFOOD_POR","INDGOOD_POR","NRJ_POR","SER_POR",
                "HICP_SWE","PFOOD_SWE","UPFOOD_SWE","INDGOOD_SWE","NRJ_SWE","SER_SWE")
colnames(HICP)<-nomenclature

HICP$X<-seq(from=1, to=dim(HICP)[1],by=1)
HICP<-HICP[order(HICP$X,decreasing=TRUE),]
HICP$X<-NULL


    # Log transformation 
HICP[,-1]<-log(HICP[,-1])



######### Inflation Year-on-Year ###############
#HICP_Finlande2 <- read.csv("data/ECB/HICP_Finlande2.csv")
#HICP_fin2 <- read.csv("data/ECB/HICP_fin2.csv")
#HICP_fin2$X<-NULL
#HICPYoY<-data.frame(HICP_Finlande2,HICP_fin2)
#colnames(HICPYoY)<-nomenclature

#HICPYoY$X<-seq(from=1, to=dim(HICPYoY)[1],by=1)
#HICPYoY<-HICPYoY[order(HICPYoY$X,decreasing=TRUE),]
#HICPYoY$X<-NULL



# Unemployment rate

UNPLOY <- read.csv("data/ECB/unemploymentrate.csv")
colnames(UNPLOY)<-c("time","UNPLOY_AUS","UNPLOY_BEL","UNPLOY_GER","UNPLOY_DEN","UNPLOY_SPA",
                    "UNPLOY_FIN","UNPLOY_FRA","UNPLOY_UKI","UNPLOY_GRE","UNPLOY_IRL",
                    "UNPLOY_ITA","UNPLOY_LUX","UNPLOY_NET","UNPLOY_POR","UNPLOY_SWE")
UNPLOY$X<-seq(from=1, to=dim(UNPLOY)[1],by=1)
UNPLOY<-UNPLOY[order(UNPLOY$X,decreasing=TRUE),]
UNPLOY$X<-NULL

    # /100 transformation

UNPLOY[,-1]<-UNPLOY[,-1]*(1/100)


# Exchange rate USD/EUR
exchangerate <- read.csv("data/ECB/Exchangerate.csv")
colnames(exchangerate)<-c("time","EURUSD_ROW")
exchangerate$X<-seq(from=1, to=dim(exchangerate)[1],by=1)
exchangerate<-exchangerate[order(exchangerate$X,decreasing=TRUE),]
exchangerate$X<-NULL
exchrate<-rbind(as.matrix(rep(NA,105)),as.matrix(exchangerate[,2]))


# Merge of monthly ECB data
df <-data.frame(HICP[37:339,],UNPLOY[85:387,])
df$time.1<-NULL
df2<-data.frame(df,exchrate)
rownames(df)<-NULL
rownames(df2)<-NULL

#data from Jan 1990
save(df2, file="data/ECB/ecb.RData")


# Differentiation of the database / starting date : Feb 1990
difdf2 <- tail(df2[,-1],-1) - head(df2[,-1],-1)
save(difdf2, file="data/ECB/difecb.RData")



# In order to add variable not available in quarterly frequency / starting date Q1 1990
dfts <- ts(df[,-1], ,start=c(1990,1),frequency=12)
dfq <- aggregate(dfts, FUN=sum, nfrequency=4)/3
dfecb<-data.frame(dfq)

save(dfecb, file="data/Quarterly/ECB/ecbq.RData")

