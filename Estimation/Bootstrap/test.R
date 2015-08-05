library('glmnet')
library('MASS')
library('parallel')

source("laurent/lasso.R")
source("Functions/lahiriboot2.R")
source("Functions/datagen.R")
source("Functions/fun.R")
source("Functions/lahiri.R")
source("Functions/montecarlo.R")


iter=10
boot=10
n=100
beta=1
p=50
nonzero=5
Q<-MC(iter,p,5,beta,n,boot,0.1,"iid5")
rowMeans(Q)

