Database for LASSOVAR ESTIMATION
========================================================

We use 5 type of databse : 
- AWM
- IMF (IFS)
- ECB
- EUROSTAT
- DATASTREAM



#### AWM Database :

Open the AWM data : 

```r
data <- read.csv("Agrege/Base/AWM/AWM18UP14.csv")
```

```
## Warning in file(file, "rt"): impossible d'ouvrir le fichier 'Agrege/Base/
## AWM/AWM18UP14.csv' : No such file or directory
```

```
## Error in file(file, "rt"): impossible d'ouvrir la connexion
```


Creation of the variable "compensation per employee" : 

```r
data$CPE=data$WIN/data$LEN
```

```
## Error in data$WIN: objet de type 'closure' non indiçable
```

Extracting subset of AWM data and creating a ts object

```r
subset<-subset(data, select=c("COMPR", "PCOMU", "POILU", "HEG", "HEXSA", "HICPSA", "HICP","EEN", "EXR", "STN", "LTN" , "URX", "YER", "XTR", "MTR", "CPE", "YWRX"))
```

```
## Error in subset.default(data, select = c("COMPR", "PCOMU", "POILU", "HEG", : l'argument "subset" est manquant, avec aucune valeur par défaut
```

```r
subsets<-ts(subset,frequency=4, start=1970)
```

```
## Error in ts(subset, frequency = 4, start = 1970): l'objet n'est pas une matrice
```


Making transformation required :

```r
COMPR<-log(subsets[,"COMPR"])
```

```
## Error in eval(expr, envir, enclos): objet 'subsets' introuvable
```

```r
POILU<-log(subsets[,"POILU"])
```

```
## Error in eval(expr, envir, enclos): objet 'subsets' introuvable
```

```r
PCOMU<-log(subsets[,"PCOMU"])
```

```
## Error in eval(expr, envir, enclos): objet 'subsets' introuvable
```

```r
HEG<-log(subsets[,"HEG"])
```

```
## Error in eval(expr, envir, enclos): objet 'subsets' introuvable
```

```r
HEXSA<-log(subsets[,"HEXSA"])
```

```
## Error in eval(expr, envir, enclos): objet 'subsets' introuvable
```

```r
HICPSA<-log(subsets[,"HICPSA"])
```

```
## Error in eval(expr, envir, enclos): objet 'subsets' introuvable
```

```r
HICP<-log(subsets[,"HICP"])
```

```
## Error in eval(expr, envir, enclos): objet 'subsets' introuvable
```

```r
EEN<-subsets[,"EEN"]
```

```
## Error in eval(expr, envir, enclos): objet 'subsets' introuvable
```

```r
EXR<-subsets[,"EXR"]
```

```
## Error in eval(expr, envir, enclos): objet 'subsets' introuvable
```

```r
URX<-subsets[,"URX"]
```

```
## Error in eval(expr, envir, enclos): objet 'subsets' introuvable
```

```r
STN<-subsets[,"STN"]/100
```

```
## Error in eval(expr, envir, enclos): objet 'subsets' introuvable
```

```r
LTN<-subsets[,"LTN"]/100
```

```
## Error in eval(expr, envir, enclos): objet 'subsets' introuvable
```

```r
YWRX<-log(subsets[,"YWRX"])
```

```
## Error in eval(expr, envir, enclos): objet 'subsets' introuvable
```

```r
YER<-log(subsets[,"YER"])
```

```
## Error in eval(expr, envir, enclos): objet 'subsets' introuvable
```

```r
CPE<-log(subsets[,"CPE"])
```

```
## Error in eval(expr, envir, enclos): objet 'subsets' introuvable
```

```r
MTR<-log(subsets[,"MTR"])
```

```
## Error in eval(expr, envir, enclos): objet 'subsets' introuvable
```

```r
XTR<-log(subsets[,"XTR"])
```

```
## Error in eval(expr, envir, enclos): objet 'subsets' introuvable
```



#### ECB Database :


***Monetary Agregate :***

```r
dataMagreg <- read.csv("Agrege/Base/ECB/dataMagreg.csv", sep=";")
```

```
## Warning in file(file, "rt"): impossible d'ouvrir le fichier 'Agrege/Base/
## ECB/dataMagreg.csv' : No such file or directory
```

```
## Error in file(file, "rt"): impossible d'ouvrir la connexion
```

```r
dataMagreg$X<-NULL
```

```
## Error in dataMagreg$X <- NULL: objet 'dataMagreg' introuvable
```

```r
monthly <- ts(dataMagreg,start=c(1970,1),frequency=12)
```

```
## Error in is.data.frame(data): objet 'dataMagreg' introuvable
```

```r
quarterly <- aggregate(monthly, nfrequency=4)/3
```

```
## Error in aggregate(monthly, nfrequency = 4): objet 'monthly' introuvable
```

```r
logquarterly <- log(quarterly)
```

```
## Error in eval(expr, envir, enclos): objet 'quarterly' introuvable
```

```r
M1<-logquarterly[,"M1"]
```

```
## Error in eval(expr, envir, enclos): objet 'logquarterly' introuvable
```

```r
M3<-logquarterly[,"M3"]
```

```
## Error in eval(expr, envir, enclos): objet 'logquarterly' introuvable
```

***Loans :***

```r
loans <- read.csv("Agrege/Base/ECB/loans.csv", sep=";")
```

```
## Warning in file(file, "rt"): impossible d'ouvrir le fichier 'Agrege/Base/
## ECB/loans.csv' : No such file or directory
```

```
## Error in file(file, "rt"): impossible d'ouvrir la connexion
```

```r
loans$date<-NULL
```

```
## Error in loans$date <- NULL: objet 'loans' introuvable
```

```r
monthly2 <- ts(loans[2:196,],start=c(1997,10),frequency=12)
```

```
## Error in is.data.frame(data): objet 'loans' introuvable
```

```r
quarterly2 <- aggregate(monthly2, nfrequency=4)/3
```

```
## Error in aggregate(monthly2, nfrequency = 4): objet 'monthly2' introuvable
```

```r
logquarterly2 <- log(quarterly2)
```

```
## Error in eval(expr, envir, enclos): objet 'quarterly2' introuvable
```

```r
LFI<-logquarterly2[,"LFI"]
```

```
## Error in eval(expr, envir, enclos): objet 'logquarterly2' introuvable
```

```r
LHO<-logquarterly2[,"LHO"]
```

```
## Error in eval(expr, envir, enclos): objet 'logquarterly2' introuvable
```


***Price Producer Index :***

```r
PPI <- read.csv("Agrege/Base/ECB/PPI.csv")
```

```
## Warning in file(file, "rt"): impossible d'ouvrir le fichier 'Agrege/Base/
## ECB/PPI.csv' : No such file or directory
```

```
## Error in file(file, "rt"): impossible d'ouvrir la connexion
```

```r
monthly3 <- ts(rev(PPI[,2]),start=c(1981,1),frequency=12)
```

```
## Error in rev(PPI[, 2]): objet 'PPI' introuvable
```

```r
quarterly3 <- aggregate(monthly3, nfrequency=4)/3
```

```
## Error in aggregate(monthly3, nfrequency = 4): objet 'monthly3' introuvable
```

```r
PPI <- log(quarterly3)
```

```
## Error in eval(expr, envir, enclos): objet 'quarterly3' introuvable
```




#### IMF Database :

Libor 3 month (United States) :

```r
LIB3M <- read.csv("Agrege/Base/IMF/LIB3M.csv")
```

```
## Warning in file(file, "rt"): impossible d'ouvrir le fichier 'Agrege/Base/
## IMF/LIB3M.csv' : No such file or directory
```

```
## Error in file(file, "rt"): impossible d'ouvrir la connexion
```

```r
LIB<-ts(LIB3M,start=c(1970,1),frequency=4)/100
```

```
## Error in is.data.frame(data): objet 'LIB3M' introuvable
```


#### EUROSTAT Database :

Economic sentiment indicator : 

```r
ESIdata <- read.csv("Agrege/Base/EUROSTAT/ESIdata.csv")
```

```
## Warning in file(file, "rt"): impossible d'ouvrir le fichier 'Agrege/Base/
## EUROSTAT/ESIdata.csv' : No such file or directory
```

```
## Error in file(file, "rt"): impossible d'ouvrir la connexion
```

```r
indiceco<-ESIdata$Value
```

```
## Error in eval(expr, envir, enclos): objet 'ESIdata' introuvable
```

```r
ESIts<-ts(rev(indiceco),start=c(1985,1),frequency=12)
```

```
## Error in rev(indiceco): objet 'indiceco' introuvable
```

```r
ESIq<-aggregate(ESIts, nfrequency=4)/3
```

```
## Error in aggregate(ESIts, nfrequency = 4): objet 'ESIts' introuvable
```

```r
ESI <- ESIq/100
```

```
## Error in eval(expr, envir, enclos): objet 'ESIq' introuvable
```

#### DATASTREAM Database :

```r
dowEuroStoxx <- read.csv("Agrege/Base/Datastream/dowEuroStoxx.csv", sep=";", dec=",")
```

```
## Warning in file(file, "rt"): impossible d'ouvrir le fichier 'Agrege/Base/
## Datastream/dowEuroStoxx.csv' : No such file or directory
```

```
## Error in file(file, "rt"): impossible d'ouvrir la connexion
```

```r
DJES<-ts(dowEuroStoxx[1:108,2],start=c(1987,1),frequency=4)
```

```
## Error in is.data.frame(data): objet 'dowEuroStoxx' introuvable
```

```r
DJES<-log(DJES)
```

```
## Error in eval(expr, envir, enclos): objet 'DJES' introuvable
```

### Merge of the different variable into a dataframe


```r
vardata<-as.ts(cbind(COMPR, PCOMU, POILU, HEG, HEXSA, HICPSA, HICP,EEN, EXR, STN, LTN , URX, YER, XTR, MTR, CPE, YWRX, LHO, LFI , M1 , M3 ,ESI, LIB, PPI, DJES ))
```

```
## Error in cbind(COMPR, PCOMU, POILU, HEG, HEXSA, HICPSA, HICP, EEN, EXR, : objet 'COMPR' introuvable
```

```r
vardataframe<-data.frame(vardata[1:176,])
```

```
## Error in data.frame(vardata[1:176, ]): objet 'vardata' introuvable
```





