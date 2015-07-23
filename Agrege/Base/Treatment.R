#### AWM DATABASE ####

data <- read.csv("Agrege/Base/AWM/AWM18UP14.csv")
data$X<-NULL
#data$date<-seq(as.Date("1970/1/1"), by = "quarter", length.out = nrow(data))

data[,-which(names(data) %in% c("date","URX","EEN","EXR","HEGWEI","CAN_YEN","NFN_YEN","URX","LTN","STN","ULC"))]<-log(data[,-which(names(data) %in% c("date","EXR","URX","EEN","HEGWEI","CAN_YEN","NFN_YEN","URX","LTN","STN","ULC"))])
data[,which(names(data) %in% c("LTN","STN"))]<-data[,which(names(data) %in% c("LTN","STN"))]/100

datats<-ts(data,start=c(1970,1),frequency=4)


#### ECB DATABASE ####

#Monetary Agregate
M1 <- read.csv("Agrege/Base/ECB/M1.csv")
M3 <- read.csv("Agrege/Base/ECB/M3.csv")

M1$M3<-M3[,2]
M1$t<-seq(from=1, to=dim(M1)[1],by=1)
M1<-M1[order(M1$t,decreasing=TRUE),]
M1$t<-NULL
colnames(M1)<-c("time","M1","M3")
rownames(M1)<-NULL
dataMagreg<-M1

monthly <- ts(dataMagreg,start=c(1980,1),frequency=12)
quarterly <- aggregate(monthly, nfrequency=4)/3
logquarterly <- log(quarterly)
M1<-logquarterly[,"M1"]
M3<-logquarterly[,"M3"]

#M1<-data.frame(logquarterly[,"M1"])
#M3<-data.frame(logquarterly[,"M3"])
#M1$date<-seq(as.Date("1980/1/1"), by = "quarter", length.out = nrow(M1))
#M3$date<-seq(as.Date("1980/1/1"), by = "quarter", length.out = nrow(M3))

#Loans
loans <- read.csv("Agrege/Base/ECB/loans.csv", sep=";")
loans$date<-NULL

monthly2 <- ts(loans[2:196,],start=c(1997,10),frequency=12)
quarterly2 <- aggregate(monthly2, nfrequency=4)/3
logquarterly2 <- log(quarterly2)
LFI<-logquarterly2[,"LFI"]
LHO<-logquarterly2[,"LHO"]
#LFI<-data.frame(logquarterly2[,"LFI"])
#LHO<-data.frame(logquarterly2[,"LHO"])
#LFI$date<-seq(as.Date("1997/10/1"), by = "quarter", length.out = nrow(LFI))
#LHO$date<-seq(as.Date("1997/10/1"), by = "quarter", length.out = nrow(LHO))


#Price Producer Index
PPI <- read.csv("Agrege/Base/ECB/PPI.csv")

PPI$t<-seq(from=1, to=dim(PPI)[1],by=1)
PPI<-PPI[order(PPI$t,decreasing=TRUE),]
PPI$t<-NULL
colnames(PPI)<-c("time","PPI")
rownames(PPI)<-NULL

monthly3 <- ts(PPI[,2],start=c(1990,1),frequency=12)
quarterly3 <- aggregate(monthly3, nfrequency=4)/3
PPI <- log(quarterly3)

#PPI<-data.frame(PPI)
#PPI$date<-seq(as.Date("1990/1/1"), by = "quarter", length.out = nrow(PPI))


# HICP

HICP <- read.csv("Agrege/Base/ECB/HICP.csv")
HICP$x<-seq(from=1, to=dim(HICP)[1],by=1)
HICP<-HICP[order(HICP$x,decreasing=TRUE),]
HICP$X<-NULL
HICP$x<-NULL
colnames(HICP)<-c("PROFOOD","NRJ","HICP_EZ","UNFOOD","INDGOOD","SERVI")
HICPts<-ts(HICP,start=c(1990,1),frequency=12)
HICPq <- aggregate(HICPts, nfrequency=4)/3
HICP<-log(HICPq)





#### IMF ####
LIB3M <- read.csv("Agrege/Base/IMF/LIB3M.csv")
LIB<-ts(LIB3M,start=c(1970,1),frequency=4)/100
#LIB<-data.frame(LIB)
#LIB$date<-seq(as.Date("1970/1/1"), by = "quarter", length.out = nrow(LIB))




#### EUROSTAT ####

# Economic sentiment indicator
ESIdata <- read.csv("Agrege/Base/EUROSTAT/ESIdata.csv")
indiceco<-ESIdata$Value
ESIts<-ts(rev(indiceco),start=c(1985,1),frequency=12)

ESIq<-aggregate(ESIts, nfrequency=4)/3
ESI <- ESIq/100
#ESI<-data.frame(ESI)
#ESI$date<-seq(as.Date("1985/1/1"), by = "quarter", length.out = nrow(ESI))



#### DATASTREAM ####
dowEuroStoxx <- read.csv("Agrege/Base/Datastream/dowEuroStoxx.csv", sep=";", dec=",")
DJES<-ts(dowEuroStoxx[1:108,2],start=c(1987,1),frequency=4)
DJES<-log(DJES)
#DJES<-data.frame(DJES)
#DJES$date<-seq(as.Date("1987/1/1"), by = "quarter", length.out = nrow(DJES))



#### FUSION BASE ####


total <- as.ts(cbind(datats,LHO, LFI , M1 , M3 ,ESI, LIB, PPI, DJES, HICP ))
var<-data.frame(total)
var$date<-seq(as.Date("1970/1/1"), by = "quarter", length.out = nrow(var))

save(var,file="Agrege/Base/vardata.Rdata")







