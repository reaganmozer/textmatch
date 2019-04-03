#' Create a data frame of pairs of documents obtained through
#' coarsened exact matching (CEM) within a specified number of bins
#' and return indices for matched sets
#'
#' @import progress
#' @param rep.list a named list of representations
#' @param Z A logical or binary vector indicating treatment and control for each unit in the study.
#' TRUE or 1 represents a treatment unit, FALSE of 0 represents a control unit.
#' @param propensity.method Either GLM or MNIR for propensity score estimation
#' @return A \link{data.frame} of indices for matched pairs of documents
#' @export
#'


calculate_pair_distances <- function(rep.list, Z, propensity.method=NULL,
                                     include=c("cosine", "euclidean","mahalanobis","propensity"),
                                     exclude=c("jaccard")){
  
  n.reps = length(rep.list)
  if(!is.null(names(rep.list))){
    rep.names = names(rep.list)
  }
  else if (is.null(names(rep.list))){
    rep.names=paste("rep",1:n.reps,sep=".")
  }
  
  if (is.null(propensity.method)){pm=NULL}
  else if (!is.null(propensity.method)){
    pm = propensity.method
  }
  inc = include
  ex = exclude
  
  #pb <- progress::progress_bar$new(total = 120,width=60, clear=FALSE,show_after=0)
  #progress::pb$tick(0)
  all.dists = pair_distances(rep.list[[1]],Z,propensity.method=pm,
                             include = inc, exclude = ex)
  all.dists = all.dists[,1:2]
  names(all.dists)[-c(1:2)]=paste(rep.names[1], names(all.dists)[-c(1:2)],sep=".")
  
  for (j in 2:n.reps){
    tmp = pair_distances(rep.list[[j]], Z, propensity.method=pm,
                         include = inc, exclude = ex)
    names(tmp)[-c(1:2)]=paste(rep.names[j], names(tmp)[-c(1:2)],sep=".")
    all.dists = merge(all.dists, tmp, by=c("index.0", "index.1"))
    }
  
  rm(rep.names, tmp, pb,ex,inc,pm)
  return(all.dists)
  
}