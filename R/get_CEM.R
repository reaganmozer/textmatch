
#' Create a data frame of pairs of documents obtained through
#' coarsened exact matching (CEM) within a specified number of bins
#' and return indices for matched sets
#'
#' @importFrom stats median
#' @param x a text representation
#' @param Z a vector of treatment indicators
#' @param rep.name a string or character with the name of the matching method
#' @param cuts a function for how the variables will be binned. Defaults to "median"
#' @return A \link{data.frame} of indices for matched pairs of documents
#' @export
#'

get_CEM <- function(x, Z, rep.name, cuts="binary", caliper_fun=NULL){
  
  stopifnot(cuts%in%c("median", "mean", "binary"))
  tmp0 = data.frame(Z, row.names=1:length(Z))
  tmp = quanteda::convert(x,to="data.frame")
  tmp = tmp[,-c(1)]
  rownames(tmp)=1:nrow(tmp)
  
  if (cuts=="binary"){
    r=matrix(2,nrow=ncol(tmp),ncol=1,dimnames=list(colnames(tmp),NULL))
  }
  else if (cuts=="median"){
    r = as.matrix(cbind(-Inf, apply(tmp,2,median), Inf), dimnames=list(colnames(tmp),NULL))
  }
  else if (cuts=="mean"){
    r = as.matrix(cbind(-Inf, apply(tmp,2,mean), Inf), dimnames=list(colnames(tmp),NULL))
  }
  cutvals = split(r,rownames(r))
  tmp=as.data.frame(tmp)
  tmp$Z=Z
  match = cem::cem("Z",data=tmp,cutpoints=cutvals,keep.all=F)
  
  if (match$n.strata==1){stop("Matching failed. No matches identified.")}
  else{
  tmp2=as.data.frame(cbind(Z,match=match$mstrata), row.names=1:length(Z))
  tmp2$match[is.na(tmp2$match)]=paste("nm",1:sum(is.na(tmp2$match)),sep="")
  dist = optmatch::exactMatch(Z~match,data=tmp2)
  if(!is.null(caliper_fun)){dist=dist+caliper_fun}
  match2 = optmatch::fullmatch(dist, min.controls=0, max.controls=1,data=tmp2)
  m1=makeMatches(match2, Z)
  m1$metric=paste(rep.name,".cem",sep="")
  
  rm(tmp0, match,tmp,r)
  
  return(m1)}
}


