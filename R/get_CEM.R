
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

get_CEM <- function(x, Z, rep.name, cuts, caliper_fun, verbose=FALSE, SR=NULL){
  

  x = as.data.frame(x)
  rownames(x)=1:nrow(x)
  if (SR==TRUE){
    tmp = x[,-ncol(x)]
    SR.vals = as.numeric(x[,ncol(x)])
    SR.val1 = 1*(SR.vals>=as.numeric(median(SR.vals)))
    q = as.numeric(quantile(x[,ncol(x)], c(1/3,2/3)))
    SR.val2 = 1*(SR.vals>=q[1]) + 1*(SR.vals>=q[2])
    q2 = as.numeric(quantile(x[,ncol(x)], c(0.25,0.5,0.75)))
    SR.val3 = 1*(SR.vals>=q2[1]) + 1*(SR.vals>=q2[2])+ 1*(SR.vals>=q2[3])
    rm(SR.vals,q,q2)
  }
  else if (SR==FALSE){
    tmp = x
    SR.val1 = SR.val2 = SR.val3 = 0
  }
  n = ncol(tmp)
  c1 = cuts[1]
  c2 = 0.05
  c3 = 0.01
  if(length(cuts)==2){c2 = cuts[2]}
  if(length(cuts)==3){c3 = cuts[3]}
  tmp1 = tmp
  tmp2 = tmp
  tmp3 = tmp
  for (j in 1:n){
    tmp1[,j]=1*(tmp[,j]>=c1)
    tmp2[,j]=1*(tmp[,j]>=c2)+1*(tmp[,j]>=c1)
    tmp3[,j]=1*(tmp[,j]>=c1)+1*(tmp[,j]>=c2)+1*(tmp[,j]>=c3)
  }
  if (length(cuts)==1){
    tmp.m = tmp1
    SR.val = SR.val1
  }
  else if (length(cuts)==2){
    tmp.m=tmp2
    SR.val = SR.val2
  }
  else if (length(cuts)==3){
    tmp.m=tmp3
    SR.val = SR.val3
  }
  f1 = sapply(1:nrow(tmp.m), function(x) paste(as.character(tmp.m[x,1:n]), collapse="."))
  f1 = as.numeric(as.factor(f1))
  tmp.out = data.frame(Z=Z, group=f1)
  if (SR==TRUE){
    tmp.out$SR = SR.val
  }
  dist = optmatch::exactMatch(Z~., data=tmp.out)
  if (!is.null(caliper_fun)){
    dist = dist + caliper_fun
  }  
  match = optmatch::fullmatch(dist, data=tmp.out, tol=0, min.controls=0, max.controls=1)
  m1=makeMatches(match, Z)
  rm(f1,dist,match)
  if (verbose==TRUE){print(paste("Identified ", nrow(m1), " matched pairs of documents.",sep=""))}
  m1$metric=paste(rep.name,".cem",sep="")
  m1
}

