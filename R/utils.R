#' Utility functions for textmatch objects
#' @param v a vector of covariate or outcome data
#' @return z a vector of treatment indicators
#' @export
#' 
SE = function(v,z){
  out=sqrt(var(v[z==1], na.rm=TRUE)/sum(z==1)+var(v[z==0], na.rm=TRUE)/sum(z==0))
  return(out)
}