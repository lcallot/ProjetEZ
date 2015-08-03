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

edfAR1(10,1,0.9,100,500,0.05)

edfiid1(10,1,4,100,500,0.05)

edfiid4(10,4,4,2,-5,1,100,500,0.05)



# Simulated data

# iid variable
iid1()
iid5(10,2,4,5,-3,-1,100)


# lahiri bootstrap
lahiriboot(df,1000,0.05,nonzero)


# lahiri empirical edf
lahiri(df,0.05)






