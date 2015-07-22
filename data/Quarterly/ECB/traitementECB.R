            ## ECB Database (Quarterly) ##


#Compensation per employee
compensation <- read.csv("data/Quarterly/ECB/compensation.csv")

compensation$x<-seq(from=1, to=dim(compensation)[1],by=1)
compensation<-compensation[order(compensation$x,decreasing=TRUE),]
compensation$x<-NULL

# Database available from Q1 1990
compensation<-compensation[41:141,]
colnames(compensation)<-c("time","COMP_AUS","COMP_BEL","COMP_GER","COMP_DEN","COMP_SPA",
  "COMP_FIN","COMP_FRA","COMP_UKI","COMP_GRE","COMP_IRL",
  "COMP_ITA","COMP_LUX","COMP_NET","COMP_POR","COMP_SWE")
compensation$time<-NULL

    # log transformation

compensation<-log(compensation)




# GPD in millions (domestic currency)
GDP <- read.csv("data/Quarterly/ECB/GDP.csv")
colnames(GDP)<-c("time","GDP_AUS","GDP_FIN","GDP_FRA","GDP_IRL","GDP_ITA",
                 "GDP_LUX","GDP_NET","GDP_POR","GDP_SWE","GDP_BEL",
                 "GDP_GER","GDP_DEN","GDP_SPA","GDP_UKI","GDP_GRE")
GDP$x<-seq(from=1, to=dim(GDP)[1],by=1)
GDP<-GDP[order(GDP$x,decreasing=TRUE),]
GDP$x<-NULL

GDP<-GDP[141:241,]
GDP$time<-NULL

    # log transformation

GDP<-log(GDP)




# Productivity
productivity <- read.csv("data/Quarterly/ECB/productivity.csv")
colnames(productivity)<-c("time","PRODPE_AUS","PRODPE_BEL","PRODPE_GER","PRODPE_DEN","PRODPE_SPA",
                          "PRODPE_FIN","PRODPE_FRA","PRODPE_UKI","PRODPE_GRE","PRODPE_IRL",
                          "PRODPE_ITA","PRODPE_LUX","PRODPE_NET","PRODPE_POR","PRODPE_SWE")
productivity$x<-seq(from=1, to=dim(productivity)[1],by=1)
productivity<-productivity[order(productivity$x,decreasing=TRUE),]
productivity$x<-NULL
productivity<-productivity[41:141,]

    # log transformation

productivity[,-1]<-log(productivity[,-1])



#DowJones EURONEXT
DJES <- read.csv("data/Quarterly/ECB/DJES.csv")
DJES$x<-seq(from=1, to=dim(DJES)[1],by=1)
DJES<-DJES[order(DJES$x,decreasing=TRUE),]
DJES$x<-NULL
DJES<-DJES[13:113,-3]
DJES$X<-NULL
names(DJES)<-"DJES_EZ"

    # log transformation

DJES<-log(DJES)





#NEER
NEER <- read.csv("data/Quarterly/ECB/NEER.csv")
NEER$x<-seq(from=1, to=dim(NEER)[1],by=1)
NEER<-NEER[order(NEER$x,decreasing=TRUE),]
NEER$x<-NULL
NEER$X<-NULL
NEER<-rbind(as.matrix(rep(NA,12)),as.matrix(NEER),NA)
NEER<-data.frame(NEER)
names(NEER)<-"NEER_EZ"

    # log transformation

NEER<-log(NEER)




# Monthly variables from ECB datawarehouse : CPI, unemployment & EXR EUR/USD
load("data/Quarterly/ECB/ecbq.RData")



# Merge

dfecbq<-data.frame(productivity,GDP,compensation,DJES,NEER,dfecb)

#save database from Q1 1990
save(dfecbq, file="data/Quarterly/ECB/dfecbq.RData")


#save database from Q2 1990
difdfecbq <- tail(dfecbq[,-1],-1) - head(dfecbq[,-1],-1)
save(difdfecbq, file="data/Quarterly/ECB/difdfecbq.RData")

