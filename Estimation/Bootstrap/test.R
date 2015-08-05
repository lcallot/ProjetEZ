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
beta=1
p=50
nonzero=5
mc1<-MC(iter,p,5,beta,n,boot,0.1,"iid5")
save(mc1,file = "Estimation/Bootstrap/mc.R")

p=100
mc2<-MC(iter,p,5,beta,n,boot,0.1,"iid5")
save(mc2,file = "Estimation/Bootstrap/mc.R")


p=200
mc3<-MC(iter,p,5,beta,n,boot,0.1,"iid5")
save(mc3,file = "Estimation/Bootstrap/mc.R")






