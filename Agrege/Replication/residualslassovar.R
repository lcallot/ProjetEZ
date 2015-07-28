residuals.lassovar <- function(object,...){
  if(!(class(object)=='lassovar'))stop('Object is not of the lassovar class')
  
  res <- object$y-cbind(1,object$x)%*%object$coefficients
  
  return(res)
}