library('glmnet')
library('MASS')
library('parallel')

source("laurent/lasso.R")
source("Functions/lahiriboot2.R")
source("Functions/datagen.R")
source("Functions/fun.R")
source("Functions/lahiri.R")
source("Functions/montecarlo.R")


iter=1000
boot=100
n=100
beta=0.9
p=50
nonzero=1
mc1<-MC(iter,p,nonzero,beta,n,boot,0.1,"AR1")
save(mc1,file = "Estimation/Bootstrap/mc1.Rdata")

p=100
mc2<-MC(iter,p,nonzero,beta,n,boot,0.1,"AR1")
save(mc2,file = "Estimation/Bootstrap/mc2.Rdata")


p=200
mc3<-MC(iter,p,nonzero,beta,n,boot,0.1,"AR1")
save(mc3,file = "Estimation/Bootstrap/mc3.Rdata")

beta=1
p=50
nonzero=3
mc4<-MC(iter,p,nonzero,beta,n,boot,0.1,"AR3")
save(mc4,file = "Estimation/Bootstrap/mc4.Rdata")

p=100
mc5<-MC(iter,p,nonzero,beta,n,boot,0.1,"AR3")
save(mc5,file = "Estimation/Bootstrap/mc5.Rdata")


p=200
mc6<-MC(iter,p,nonzero,beta,n,boot,0.1,"AR3")
save(mc6,file = "Estimation/Bootstrap/mc6.Rdata")




