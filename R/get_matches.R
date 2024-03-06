#' Similarity and distance computation between documents or features
#'
#' These functions compute distance matrices from a text representation where each row is a document
#' and each column is a feature to measure distance over based on treatment indicator Z
#'
#' @param x a matrix of pairwise distances for all potential matches of treatment and control units. See \link{pair_distances}.
#' @param Z a vector of treatment indicators
#' @param dist.name a string or character with the name of the matching method
#' @param caliper_fun an optional function specifying the caliper to enforce when matching
#' @param text_caliper should a text-based caliper be enforced? If so, specify the caliper (as a quantile of the distribution of pairwise document distances) 
#' @export

get_matches <- function(x, Z, dist.name, caliper_fun, text_caliper=0.001, tol=0){
  dist=x
  tmp0 = data.frame(Z)
  rownames(tmp0)=1:nrow(tmp0)
  
  txt.calip=NULL
  if (!is.null(text_caliper) & text_caliper>0){
    calip.val = stats::quantile(as.vector(dist), text_caliper,na.rm=T)
    txt.calip = optmatch::caliper(dist,width=calip.val)
  }
  
  
  if(is.null(caliper_fun)){
    dist2 = dist+txt.calip
  }
  else if (!is.null(caliper_fun)){
    dist2 = dist + txt.calip + caliper_fun
  }
  match = optmatch::fullmatch(dist2,data=tmp0,tol=tol)
  m1 = data.frame(Z, match=match)
  m1$ID = 1:nrow(m1)
  m1 = m1[!is.na(m1$match),]
  m1 = m1[with(m1,order(match, Z)),]
  m1.t = m1[m1$Z==1,-c(1)]
  m1.c = m1[m1$Z==0,-c(1)]
  m2=merge(m1.t, m1.c, by="match", suffixes=c(".1", ".0"), all=T)
  m3 = subset(m2, select=-c(match))
  m3$metric=dist.name
  rm(calip.val, tmp0, match,dist2, m1, m2)
  
  return(m3)
}
