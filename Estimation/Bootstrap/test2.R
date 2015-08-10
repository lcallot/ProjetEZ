library('glmnet')
library('MASS')
library('parallel')

source("laurent/lasso.R")
source("Functions/lahiriboot3.R")
source("Functions/datagen.R")
source("Functions/fun.R")
source("Functions/montecarlo2.R")

#2
iter=1000
boot=100
n=100
p=50
nonzero=5
beta=2
N=100
alpha=0.05

nonzero=5
mcfunction2(x,p,nonzero,beta,n,boot,alpha,"iid5",N)



