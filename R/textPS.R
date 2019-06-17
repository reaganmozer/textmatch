#' Given a text representation x and treatment indicator Z,
#' estimate a univariate projection of the text data that can serve
#' as a sufficient statistic for the confounding information captured within the text.
#'
#' @importFrom stats glm
#' @param x a TDM or DFM text representation
#' @param Z a vector of treatment indicators
#' @return A \link{data.frame} MNIR sufficient reduction scores for a corpus
#' @export
#'
textPS <- function(x, Z, verbose=FALSE, g=1){
  
  if(!is.logical(verbose)) stop("verbose must be a logical.")
  if (nrow(x)!=length(Z)){x=t(as.matrix(x))}
  stopifnot(nrow(x)==length(Z))
  
  covs = as.matrix(data.frame(Z), ncol=1)
  counts = Matrix::Matrix(as.matrix(x))
  fitIR = suppressWarnings(textir::mnlm(cl=NULL, covars=covs, counts, verb=verbose, gamma=g))
  SR = textir::srproj(fitIR, counts)
  rm(fitIR,covs,counts)
  
  SR
}

#' Given a text representation x and treatment indicator Z,
#' construct a distance matrix for the pairwise distances between
#' treatment and control documents based on the textual propensity score.
#' @inheritParams textPS
#' @param normalize should the TDM features be normalized? Defaults to TRUE.
#' @param return.df Should the distances be returned as a data frame? Default is TRUE.
#' @export
#'
textPS_dist <- function(x, Z,  normalize=TRUE,
                     return.df=TRUE, verbose=FALSE){
  
  if(!is.logical(verbose)) stop("verbose must be a logical.")
  
  SR = textPS(x, Z, verbose)
  if (normalize){
    dist = optmatch::match_on(Z~SR[,1],data=data.frame(Z))
  }
  else if (!normalize){
    dist = optmatch::match_on(Z~SR,data=data.frame(Z))
  }
  
  if (return.df){
  ps = as.data.frame(reshape2::melt(dist@.Data))
  names(ps)[1:2]=c("index.0", "index.1")
  }
  if (!return.df){
    ps = dist
  }
  ps
}

