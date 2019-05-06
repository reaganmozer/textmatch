#' Create a data frame of pairs of documents obtained through
#' coarsened exact matching (CEM) within a specified number of bins
#' and return indices for matched sets
#'
#' @importFrom stats glm
#' @param x a TDM or DFM text representation
#' @param Z a vector of treatment indicators
#' @param return.df Should the distances be returned as a data frame? Default is TRUE.
#' @return A \link{data.frame} MNIR sufficient reduction scores for a corpus
#' @export
#'

get_MNIR <- function(x, Z, weighting, return.df=TRUE, verbose=TRUE){
  
  stopifnot(!is.null(weighting))
  if(!is.logical(verbose)) stop("verbose must be a logical.")
  
  covs = as.matrix(Z, ncol=1)
  counts = as.matrix(x)
  fitIR = textir::mnlm(cl=NULL, covars=covs, counts,verb=verbose)
  SR = textir::srproj(fitIR, counts)
  rm(fitIR)
  
  if (weighting==1){
    dist = optmatch::match_on(Z~SR[,1])
  }
  if (weighting==2){
    dist = optmatch::match_on(Z~SR)
  }
  if (weighting==3){
    dist = optmatch::match_on(glm(Z~SR[,1],family="binomial"))
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

