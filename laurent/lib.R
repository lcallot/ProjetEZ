
# A function to plot the data in levels and first difference side-by-side.
varplots <- function(awm,pnames,grp.lst=NULL){
  
  
  dp <- rbind(rep(NA,length(pnames)),diff(as.matrix(select_(awm,.dots=pnames)),lag=1))
  mdp <- melt(data.frame(dp,date=awm$date),id.vars = 'date')
  mdp$diff <- 'first difference'
  
  mlp <- melt(awm,measure.vars = pnames,id.vars = 'date')
  mlp$diff <- 'level'
  
  mp <- rbind(mlp,mdp)
  
  #building the group factor
  if(!is.null(grp.lst)){
    gi <- 1
    mp$grp <- 0
    for(g in grp.lst){
      mp$grp <- mp$grp + gi*mp$variable%in%g
      gi <- gi+1
    }
    mp$grp <- as.factor(mp$grp)
    levels(mp$grp) <- names(grp.lst)
  }
  
  # plotting
  gm <- ggplot(mp,aes(x=date,y=value,colour=variable,linetype=variable)) 
  gm <- gm + geom_line() + theme_bw() + theme(legend.position="bottom") 
  if(!is.null(grp.lst)) gm <- gm + facet_wrap(~ grp + diff,scales='free',ncol=2)
  if(is.null(grp.lst)) gm <- gm + facet_wrap(~ diff,scales='free',ncol=2)
  
  return(gm)
}