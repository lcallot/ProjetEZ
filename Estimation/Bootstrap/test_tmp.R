

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
