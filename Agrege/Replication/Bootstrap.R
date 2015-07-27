
load("Agrege/Base/vardata.Rdata")
# data from Q1 1990 to 2013 Q4 (81:176)
sub<-var[81:176,]
sub<- sub[,c('date','M1','M3','STN','LTN','YED','YER','URX','POILU','LHO', 'LFI')]


lm<-lm(sub$YED ~ sub$M3 + sub$YED)



