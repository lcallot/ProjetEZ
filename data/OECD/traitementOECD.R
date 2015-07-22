                  ## OECD Database ##

# Long-term interest rate
LTrate <- read.csv("data/OECD/LTrate.csv", sep=";", dec=",")
colnames(LTrate)<-c("time","LTR_AUS","LTR_BEL","LTR_DEN","LTR_FIN","LTR_FRA",
  "LTR_GER","LTR_GRE","LTR_IRL","LTR_ITA","LTR_LUX",
  "LTR_NET","LTR_POR","LTR_SPA","LTR_SWE","LTR_UKI")

    # /100 transformation
LTrate[,-1]<-LTrate[,-1]/100


# Long-term interest rate
STrate <- read.csv("data/OECD/STrate.csv", sep=";", dec=",")
colnames(STrate)<-c("time","STR_AUS","STR_BEL","STR_DEN","STR_FIN","STR_FRA",
                    "STR_GER","STR_GRE","STR_IRL","STR_ITA","STR_LUX",
                    "STR_NET","STR_POR","STR_SPA","STR_SWE","STR_UKI")

STrate[,-1]<-STrate[,-1]/100

df<-data.frame(STrate[1:303,],LTrate[1:303,])
df$time.1<-NULL
df$time<-NULL

oecd<-df

#data from Jan 1990
save(oecd, file="data/OECD/oecd.RData")


# Differentiation of the database / starting date : Feb 1990
difdf <- tail(df,-1) - head(df,-1)
save(difdf, file="data/OECD/difoecd.RData")




    
# In order to add variable not available in quarterly frequency / starting date Q1 1990
dfts <- ts(df, ,start=c(1990,1),frequency=12)
dfq <- aggregate(dfts, nfrequency=4)/3
dfq<-data.frame(dfq)

save(dfq, file="data/Quarterly/OECD/oecdq.RData")


