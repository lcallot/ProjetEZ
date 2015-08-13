library(BigVAR)
library(lattice)

load("Agrege/Base/vardata.Rdata")
df<-var[,c("date", "YER", "PCR", "GCR", "ITR", "XTR", "MTR", "YED",  
           "GCD", "ITD", "XTD", "MTD",  "WIN",  
             "HICPSA", "URX", "STN", "LTN","POILU", 
           "PCOMU", "YWR","WRN",  "EEN", "EXR",  "M1",
           "M3", "ESI", "LIB", "PPI", "DJES")]

#df<-var[,c("date", "YER", "PCR", "GCR")]

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


a=15
b=5

#load("/Users/carrierclement/Documents/Stage VU/BigVAR/BigVAR/data/Y.RData")

bvdata1<-constructModel(df3, p=16, "None",gran=c(a,b) , h = 1, cv = "Rolling")
bvdata2<-constructModel(df3, p=16, "Lag",gran=c(a,b) , h = 1, cv = "Rolling")
bvdata3<-constructModel(df3, p=16, "SparseLag",gran=c(a,b) , h = 1, cv = "Rolling")
bvdata4<-constructModel(df3, p=16, "Diag",gran=c(a,b) , h = 1, cv = "Rolling")
bvdata5<-constructModel(df3, p=16, "SparseDiag",gran=c(a,b) , h = 1, cv = "Rolling")

results1=cv.BigVAR(bvdata1)
results2=cv.BigVAR(bvdata2)
results3=cv.BigVAR(bvdata3)
results4=cv.BigVAR(bvdata4)
results5=cv.BigVAR(bvdata5)

save(results1,file="Agrege/Replication/results1.Rdata")
save(results2,file="Agrege/Replication/results2.Rdata")
save(results3,file="Agrege/Replication/results3.Rdata")
save(results4,file="Agrege/Replication/results4.Rdata")
save(results5,file="Agrege/Replication/results5.Rdata")

#load("Agrege/Replication/results1.Rdata")
#load("Agrege/Replication/results2.Rdata")
#load("Agrege/Replication/results3.Rdata")
#load("Agrege/Replication/results4.Rdata")
#load("Agrege/Replication/results5.Rdata")
#results
#str(results)
#plot(results)

#SparsityPlot.BigVAR.results(results1)
#SparsityPlot.BigVAR.results(results2)
#SparsityPlot.BigVAR.results(results3)
#SparsityPlot.BigVAR.results(results4)
#SparsityPlot.BigVAR.results(results5)

#str(results1)
#str(results2)
#str(results3)
#str(results4)
#str(results5)

#results1
#results2
#results3
#results4
#results5

