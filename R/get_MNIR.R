#' Create a data frame of pairs of documents obtained through
#' coarsened exact matching (CEM) within a specified number of bins
#' and return indices for matched sets
#'
#' @param x a TDM or DFM text representation
#' @param Z a vector of treatment indicators
#' @return A \link{data.frame} MNIR sufficient reduction scores for a corpus
#' @export
#'

get_MNIR <- function(x, Z,weighting=NULL, rep.name=NULL){
  
  stopifnot(!is.null(weighting))
  if (!is.dfm(x)){dat=quanteda::as.dfm(x)}
  if (is.dfm(x)){dat=x}
  covs = as.matrix(Z, ncol=1)
  rownames(covs)=1:nrow(covs)
  if (!is.null(rep.name)){name=rep.name}
  if (is.null(rep.name)){name = "mnir.dist"}
  
  fitIR = textir::mnlm(cl=NULL, covars=covs, counts=dat)
  SR = textir::srproj(fitIR, counts=dat)
  rm(fitIR)
  
  tmp=data.frame(Z)
  if (weighting==1){
    dist = optmatch::match_on(Z~SR[,1])
    ps = reshape2::melt(dist)
  }
  if (weighting==2){
    dist = optmatch::match_on(Z~SR)
    ps = reshape2::melt(dist)
  }
  rm(SR, dist)
  
  names(ps)[1:2]=c("index.0", "index.1")
  if (!is.null(rep.name)){names(ps)[3]=rep.name}
  ps
}

