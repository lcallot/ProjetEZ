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
iter=100
boot=200
p=10
nonzero=1
beta=1

st <- proc.time()
mc1   <-MC(iter,p,nonzero,beta,n=100,boot,0.1,"iid1")
save(mc1,file = "Estimation/Bootstrap/data/5/mc1.Rdata")
cat('\n MC 1 completed')
mc2   <-MC(iter,p,nonzero,beta,n=500,boot,0.1,"iid1")
save(mc2,file = "Estimation/Bootstrap/data/5/mc2.Rdata")
cat('\n MC 2 completed')
mc3   <-MC(iter,p,nonzero,beta,n=1000,boot,0.1,"iid1")
save(mc3,file = "Estimation/Bootstrap/data/5/mc3.Rdata")
cat('\n MC 3 completed')
mc4   <-MC(iter,p,nonzero,10*beta,n=100,boot,0.1,"iid1")
save(mc4,file = "Estimation/Bootstrap/data/5/mc4.Rdata")
cat('\n MC 4 completed')
mc5   <-MC(iter,p,nonzero,10*beta,n=500,boot,0.1,"iid1")
save(mc5,file = "Estimation/Bootstrap/data/5/mc5.Rdata")
cat('\n MC 5 completed')
mc6   <-MC(iter,p,nonzero,10*beta,n=1000,boot,0.1,"iid1")
save(mc6,file = "Estimation/Bootstrap/data/5/mc6.Rdata")
cat('\n MC 6 completed')

cat(paste0('Duration: ',round((proc.time()-st)[3],0),' seconds.'))