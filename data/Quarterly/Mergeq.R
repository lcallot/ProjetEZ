# merge of all the variables 

# OECD Data
load("data/Quarterly/OECD/oecdq.RData")


# IMF Data
load("data/Quarterly/IMF/dfimfq.RData")
dfimfq$Time<-NULL

# ECB Data
load("data/Quarterly/ECB/dfecbq.RData")


# Save database 
vardataq<-data.frame(dfecbq,dfimfq,dfq)
save(vardataq, file="data/Quarterly/vardataq.RData")

# Save diferentiated data from Q2 1990
difvardataq <- tail(vardataq[,-1],-1) - head(vardataq[,-1],-1)
save(difvardataq, file="data/Quarterly/difvardataq.RData")


