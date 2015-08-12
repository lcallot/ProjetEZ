library(BigVAR)
library(lattice)

load("Agrege/Base/vardata.Rdata")
#df<-var[,c("date", "YER", "PCR", "GCR", "ITR", "XTR", "MTR", "YED",  
#           "GCD", "ITD", "XTD", "MTD", "YFD", "YIN", "WIN", "TIN", 
#           "YFN",  "HICPSA", "URX", "STN", "LTN","POILU", 
#           "PCOMU", "YWR","WRN",  "EEN", "EXR",  "M1",
#           "M3", "ESI", "LIB", "PPI", "DJES")]

df<-var[,c("date", "YER", "PCR", "GCR",   
            "WIN", "HICPSA", "URX", "STN", "LTN","POILU", 
           "PCOMU", "YWR",  "EEN", "EXR",  "M1",
           "M3", "ESI", "LIB", "PPI", "DJES")]

Ddf <- tail(df,-1) - head(df,-1)

df<-df[81:176,]
df$date<-NULL

df2<-matrix(0,92,dim(df)[2])
for (j in 1:dim(df)[2]){
  for (i in 1:92){
    df2[i,j]<-df[i+4,j]-df[i,j]
  }
}
df2<-data.frame(df2)
colnames(df2)<-colnames(df)

df2$URX<-df$URX[-(1:4)]
df2$STN<-df$STN[-(1:4)]
df2$LTN<-df$LTN[-(1:4)]
df2$EEN<-df$EEN[-(1:4)]
df2$EXR<-df$EXR[-(1:4)]
df2$LIB<-df$LIB[-(1:4)]

df3<-as.matrix(df2,92,dim(df)[2])




#load("/Users/carrierclement/Documents/Stage VU/BigVAR/BigVAR/data/Y.RData")

bvdata<-constructModel(df3, p=4, "Lag",gran=c(50,10) , h = 1, cv = "Rolling")
results=cv.BigVAR(bvdata)

save(results,file="Agrege/Replication/results2.Rdata")


load("Agrege/Replication/results.Rdata")
results
str(results)
plot(results)
SparsityPlot.BigVAR.results(results)




