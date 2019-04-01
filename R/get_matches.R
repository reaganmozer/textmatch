#' Similarity and distance computation between documents or features
#'
#' These functions compute distance matrices from a text representation where each row is a document
#' and each column is a feature to measure distance over based on treatment indicator Z
#'
#' @param x a matrix of pairwise distances for all potential matches of treatment and control units. See \link{pair_distances}.
#' @param Z a vector of treatment indicators
#' @param dist.name a string or character with the name of the matching method
#' @param caliper_fun an optional function specifying the caliper to enforce when matching
#' @return A \link{data.frame} of matched pairs of documents
#' @export


get_matches <- function(dist, Z, dist.name, caliper_fun=NULL){

  tmp0 = data.frame(Z, row.names=1:length(Z))

  calip.val = stats::quantile(as.vector(dist), c(0.001))
  if(is.null(caliper_fun)){
    dist2 = dist+optmatch::caliper(dist,width=calip.val)
  }
  else{
    dist2 = dist + optmatch::caliper(dist,width=calip.val) + caliper_fun
  }
  match = optmatch::fullmatch(dist2,data=tmp0,
                              min.controls=0,max.controls=1)
  m1=makeMatches(match, Z)
  m1$metric=dist.name
  rm(calip.val, tmp0, match,dist2)

  return(m1)
}

#' Create a data frame of matched pairs of documents and return indices for matched sets
#'
#' @param match.obj a matched data set
#' @param Z a vector of treatment indicators
#' @return A \link{data.frame} of indices for matched pairs of documents

makeMatches <- function(match.obj, Z){
  
  d = data.frame(cbind(Z, matches=match.obj))
  rownames(d)=1:nrow(d)
  d2 = d[!is.na(d$matches),]
  d2=d2[order(d2$matches),]
  d2$matches=as.factor(d2$matches)
  levels(d2$matches)=1:length(unique(d2$matches))
  
  tmp=c()
  for (i in 1:length(unique(d2$matches))){
    index.0 = rownames(d2)[d2$matches==i & d2$Z==0]
    index.1 = rownames(d2)[d2$matches==i & d2$Z==1]
    for (k in 1:length(index.1)){
      for (j in 1:length(index.0)){
        tmp = rbind(tmp, c(index.1[k], index.0[j]))
      }
    }
  }
  tmp=as.data.frame(tmp)
  names(tmp)=c("index.0", "index.1")
  rm(d, d2, index.0, index.1)
  return(tmp)
}

