#' Applies bounds, weights, and/or coarsening schemes to a dfm or document frequency matrix
#' to reduce the dimension of the data, reduce noise, or apply other design rules
#' (e.g. - to exclude words that occur in too few or too many documents).
#'
#' @param x a matrix text representation with rows corresponding to each document in a corpus and columns
#' that represent summary measures of the text (e.g., word counts, topic proportions, etc.). Acceptable forms include
#' a valid \pkg{quanteda} \code{dfm} object, a \pkg{tm} Document-Term Matrix, or a matrix of estimated topic proportions.
#' @param bounds a vector of lower and upper bounds to enforce. Defaults to excluding any terms that appear in only one document
#' and any terms that appear in every document
#' @param tfidf optional scheme to use for weighting the DTM. Defaults to \code{FALSE}.
#' @param verbose indicator for verbosity
#' @return A bounded DFM
#' @export

transform_dfm = function(x, bounds, tfidf=FALSE, verbose=TRUE){
  if(verbose==T){print(paste("Removing any features that appear in fewer than ", bounds[1], " documents or more than ", bounds[2], " documents.",sep=""))}
  freq=quanteda::textstat_frequency(x)
  remove = unique(c( which(freq$docfreq< bounds[1]), which(freq$docfreq>bounds[2])))
  n.rm = length(remove)
  rm = freq$feature[remove]
  if(quanteda::is.dfm(x)==TRUE){dat=x}
  else{dat=as.dfm(x)}
  dat2 = quanteda::dfm_remove(dat, rm)
  if (tfidf==TRUE){dat2=quanteda::dfm_tfidf(dat2)}
  if (verbose==TRUE){
    print(paste("Removed ", n.rm, " features. This representation now contains ", ncol(dat2), " features.",sep=""))
  }
  rm(freq,remove,n.rm, rm)
  dat2
}
