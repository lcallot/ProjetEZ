#' A function to estimate a model with the Lasso and select the penalty parameter by BIC. Formula interface for glmnet.
#' 
#' @name lasso
#' 
#' @param formula an object of class formula. An intercept should always be included in the model, do not explicitely remove the intercept from the formula. 
#' @param data an optional data frame.
#' @param pweights an optional vector of penalty weights (of length r(T-r+1)) used to compute an adaptive lasso. If a weight is equal to zero the variable is automatically excluded from the model.  
#' @param alpha the mixing parameter of elastic net. Default 1 (Lasso), set to 0 for ridge regression.
#' @param lambda user-defined lambda sequence.
#' @param postest boolean: should the post Lasso OLS estimation be performed? 
#' @param pmax: maximum number of non-zero parameters to estimate.
#' @param ... unused.
#' 
#' @return An object of the parsimonious class.
#' 
#' @examples
#' \dontrun{
#' data(economics, package="ggplot2")
#' psr <- tail(economics$psavert,-1) # personal saving rate.
#' lur <- head(log(economics$unemploy/economics$pop),-1) # lagged log unemployment rate.
#' pce <- head(log(economics$pce/economics$pop),-1) # lagged log consumption expenditures per capita.
#' ptvmod <- ptvfit(psr~lur+pce)
#' }
#' 
#' @importFrom glmnet glmnet
#' @export
lasso <- function(formula, data, pweights=NULL, alpha=1, lambda=NULL, postest=TRUE, pmax = NULL,...){
	
	# checking optional param
	if(!is.numeric(alpha))stop('alpha must be between 0 and 1')
	# if (!is.null(stable) && !is.numeric(stable)) stop('stable must be a numeric vector.')
	
	# From lm with some cleaning
	cl <- match.call()
	mf <- match.call(expand.dots = FALSE)
	m <- match(c("formula", "data", "na.action"), names(mf), 0L)
	mf <- mf[c(1L, m)]
	mf$drop.unused.levels <- TRUE
	mf[[1L]] <- quote(stats::model.frame)
	mf <- eval(mf, parent.frame())	
	mt <- attr(mf, "terms")
	
	intercept <- as.logical(attr(mt, 'intercept'))
	# getting the model components
	y <- model.response(mf, "numeric")
	x <- model.matrix(mt, mf, contrasts)
	if (intercept) x <- x[,-1]
	
	# A few useful variables
	nT <- length(y)
	r <- length(x)/nT
	x <- matrix(x,nrow=nT,ncol=r)
	
  vn <- all.vars(formula)[-1]
  if(length(vn)!=ncol(x)) vn <- paste(vn,1:ncol(x),sep='_')  
	colnames(x) <- vn
  print(vn)

	# Checking the pweights for the adaptive Lasso
	if (!is.null(pweights) && !is.numeric(pweights)) 
	{stop("'pweights' must be a numeric vector.")}
		
	if(is.null(pmax))pmax <- ncol(x)
	
	if(!is.null(pweights))	{
		if(!(length(pweights)==r))stop('pweights must have length equal to number of dependent variables.')
		wvec <- matrix(pweights,ncol=1,byrow=TRUE)		
		
		if((r>1)&(length(wvec)==r*nT)) wvec<- head(wvec,-r*(r-1))
		
		# exclude infinite pweights
		exclude <- which(wvec==Inf)		
		# removind the Infs
		wvec[which(wvec==Inf)] <- 0
		fb <- glmnet(x,y,family='gaussian',intercept=intercept,standardize=FALSE,lambda=lambda,alpha=alpha,penalty.factor=wvec,exclude=exclude,pmax=pmax)
	}
	else 
	{
		penfac <- rep(1, ncol(x))
		fb <- glmnet(x,y,family='gaussian',intercept=intercept,standardize=FALSE,lambda=lambda,alpha=alpha,penalty.factor=penfac,pmax=pmax)
	}

	
	# Getting fit and residuals
	pr.fb <- predict(fb,x)	
	res	<-matrix(rep(y,ncol(pr.fb)),ncol=ncol(pr.fb))-pr.fb
	
	# Computing BIC
	RSS	<-colSums(res^2)
	if(alpha>0)bic <- log(RSS/nT) + log(nT) / nT * fb$df# * log(log(ncol(x)))
	# BIC with ridge df
	if(alpha==0){rdf <- .ridge.df(x,fb$lambda); bic <- log(RSS/nT) + log(nT)*rdf/nT}
	
	# Selecting BIC minimizing beta
	coef_las <- c(fb$a0[which.min(bic)],fb$beta[,which.min(bic)])
	names(coef_las) <- c('intercept',vn)
	# Creating the ptv object 
	las <- list('y'=y,'x'=x,'lambda'=fb$lambda[which.min(bic)],coefficients=coef_las,
              'residuals'=res[,which.min(bic)],'pweights'=pweights,'alpha'=alpha,
              'formula'=formula)
	
	# Post Lasso OLS + stderr
	if(sum(coef_las[-1]!=0)>0 & postest){
		post <- lm.fit(y=y,x=cbind(1,x[,coef_las[-1]!=0]))
		cpst <- matrix(0,nrow=length(coef_las),ncol=1)
		cpst[coef_las!=0,] <- coefficients(post)
    cpst <- as.numeric(cpst)
    names(cpst) <- names(coef_las)
		sepost <- try(sum(residuals(post)^2) *diag(solve(t(x[,coef_las[-1]!=0])%*%(x[,coef_las[-1]!=0])))/(nT-sum(coef_las[-1]!=0)))
		las$post <- list('coefficients'=cpst,'stderr'=sepost,'residuals'=residuals(post))
	}
	
	return(las)
}





