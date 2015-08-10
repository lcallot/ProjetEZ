# merge of all the variables 

# OECD Data
load("data/OECD/oecd.RData")


# IMF Data
load("data/IMF/imf.RData")
imf$time<-NULL

# ECB Data
load("data/ECB/ecb.RData")


# Save database 
vardatam<-data.frame(ecb,imf,oecd)
vardatam$STR_SWE[143]=0.0370
save(vardatam, file="data/vardatam.RData")



# Save diferentiated data from Q2 1990
difvardatam <- tail(vardatam[,-1],-1) - head(vardatam[,-1],-1)
save(difvardatam, file="data/difvardatam.RData")



