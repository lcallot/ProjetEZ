library('glmnet')
library('MASS')
library('parallel')

source("laurent/lasso.R")
source("Functions/lahiriboot2.R")
source("Functions/datagen.R")
source("Functions/fun.R")
source("Functions/lahiri.R")
source("Functions/montecarlo.R")

#2
iter=1000
boot=100
n=100
p=100
nonzero=5
beta=1
mc2<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid5")
save(mc2,file = "Estimation/Bootstrap/data/2/mc2.Rdata")
p=200
nonzero=5
mc3<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid5")
save(mc3,file = "Estimation/Bootstrap/data/2/mc3.Rdata")


