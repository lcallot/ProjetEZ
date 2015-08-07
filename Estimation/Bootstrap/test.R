library('glmnet')
library('MASS')
library('parallel')

source("laurent/lasso.R")
source("Functions/lahiriboot2.R")
source("Functions/datagen.R")
source("Functions/fun.R")
source("Functions/lahiri.R")
source("Functions/montecarlo.R")




## 1
iter=1000
boot=100
n=100

beta=0.9
p=50
nonzero=1
mc1<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid1")
save(mc1,file = "Estimation/Bootstrap/data/1/mc1.Rdata")
mc4<-MC(iter,p,nonzero,beta,n,boot,0.1,"AR1")
save(mc4,file = "Estimation/Bootstrap/data/1/mc4.Rdata")

p=100
mc2<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid1")
save(mc2,file = "Estimation/Bootstrap/data/1/mc2.Rdata")
mc5<-MC(iter,p,nonzero,beta,n,boot,0.1,"AR1")
save(mc5,file = "Estimation/Bootstrap/data/1/mc5.Rdata")

p=200
mc3<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid1")
save(mc3,file = "Estimation/Bootstrap/data/1/mc3.Rdata")
mc6<-MC(iter,p,nonzero,beta,n,boot,0.1,"AR1")
save(mc6,file = "Estimation/Bootstrap/data/1/mc6.Rdata")



## 2
iter=10
boot=10
n=100

beta=1
p=50
nonzero=5
mc1<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid5")
save(mc1,file = "Estimation/Bootstrap/data/2/mc1.Rdata")
nonzero=10
mc4<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid10")
save(mc4,file = "Estimation/Bootstrap/data/2/mc4.Rdata")

p=100
nonzero=5
mc2<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid5")
save(mc2,file = "Estimation/Bootstrap/data/2/mc2.Rdata")
nonzero=10
mc5<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid10")
save(mc5,file = "Estimation/Bootstrap/data/2/mc5.Rdata")

p=200
nonzero=5
mc3<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid5")
save(mc3,file = "Estimation/Bootstrap/data/2/mc3.Rdata")
nonzero=10
mc6<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid10")
save(mc6,file = "Estimation/Bootstrap/data/2/mc6.Rdata")




## 3
iter=1000
boot=100
n=100

beta=5
p=50
nonzero=5
mc1<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid5")
save(mc1,file = "Estimation/Bootstrap/data/3/mc1.Rdata")
nonzero=10
mc4<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid10")
save(mc4,file = "Estimation/Bootstrap/data/3/mc4.Rdata")

p=100
nonzero=5
mc2<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid5")
save(mc2,file = "Estimation/Bootstrap/data/3/mc2.Rdata")
nonzero=10
mc5<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid10")
save(mc5,file = "Estimation/Bootstrap/data/3/mc5.Rdata")

p=200
nonzero=5
mc3<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid5")
save(mc3,file = "Estimation/Bootstrap/data/3/mc3.Rdata")
nonzero=10
mc6<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid10")
save(mc6,file = "Estimation/Bootstrap/data/3/mc6.Rdata")




## 4
iter=1000
boot=100

n=400
beta=5
p=50
nonzero=5
mc1<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid5")
save(mc1,file = "Estimation/Bootstrap/data/4/mc1.Rdata")

p=100
mc2<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid5")
save(mc2,file = "Estimation/Bootstrap/data/4/mc2.Rdata")

p=200
mc3<-MC(iter,p,nonzero,beta,n,boot,0.1,"iid5")
save(mc3,file = "Estimation/Bootstrap/data/4/mc3.Rdata")

n=100
beta=1
nonzero=3
p=50
mc4<-MC(iter,p,nonzero,beta,n,boot,0.1,"AR3")
save(mc4,file = "Estimation/Bootstrap/data/4/mc4.Rdata")
p=100
mc5<-MC(iter,p,nonzero,beta,n,boot,0.1,"AR3")
save(mc5,file = "Estimation/Bootstrap/data/4/mc5.Rdata")
p=200
mc6<-MC(iter,p,nonzero,beta,n,boot,0.1,"AR3")
save(mc6,file = "Estimation/Bootstrap/data/4/mc6.Rdata")
