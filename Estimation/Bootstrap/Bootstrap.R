library('glmnet')
library('MASS')

source("laurent/lasso.R")
source("Functions/RW.R")
source("Functions/fun.R")
source("Functions/lahiri.R")
source("Functions/lahiriboot.R")
source("Functions/AR1.R")
source("Functions/edfAR1.R")
source("Functions/edfiid4.R")
source("Functions/edfiid1.R")
source("Functions/iid1.R")
source("Functions/iid5.R")

## lahiri  edf 

edfAR1(150,0.9,100,10,0.05)

edfiid1(150,1,4,100,1000,0.05)

edfiid5(150,4,4,2,-5,1,100,1000,0.05)



# Simulated data

# iid variable
df1<-iid1(10,0.9,100)
df2<-iid5(10,2,4,5,-3,-1,100)
df3<-depAR1(10,0.9,100)

funboot(df3,10,"post")

# lahiri bootstrap
lahiriboot(df1,1000,0.05,1)






