#' Generates balance diagnostics for many possible matched datasets
#' which can be used to make comparisons between matching methods.
#'
#' @param matchobj an \link{optmatch} object or a matched dataset
#' @param Z a vector of treatment indicators
#' @param covs a matrix or data frame of covariates to assess balance on
#' @return A \link{data.frame} MNIR sufficient reduction scores for a corpus
#' @export
#'
#'
#'
get_bal_optmatch = function(matchobj, Z, covs){
  
  n.ess = summary(matchobj)$effective.sample.size
  bal = cobalt::bal.tab(matchobj, covs=covs, treat=Z,s.d.denom="pooled")
  n.t = bal$Observations[2,2]
  n.c = bal$Observations[2,1]
  
  n.covs = ncol(covs)
  n.bad = length(which(abs(bal$Balance$Diff.Adj)>0.1)) # Number of covariates with "large" imbalances remaining
  n.good = n.covs - n.bad
  
  val0 = sum(abs(bal$Balance$Diff.Adj)) # Sum of standardized differences in means
  val1=  mean(abs(bal$Balance$Diff.Adj)) # Overall mean of standardized differences in means
  val2 = mean(abs(bal$Balance$Diff.Adj[bal$Balance$Type=="Contin."])) # Average standardized difference in means for continous variables
  val3 = mean(abs(bal$Balance$Diff.Adj[bal$Balance$Type!="Contin."])) # Average standardized difference in means for binary variables
  val4 = max(abs(bal$Balance$Diff.Adj)) # Max difference in standardized means after adjustment
  
  out = data.frame(ESS = round(n.ess), 
                   num.covs=n.covs, 
                   num.balanced = n.good, 
                   sum.diff.adj = round(val0,3),
                   mean.diff.adj = round(val1,3),
                   max.diff.adj = round(val4,3))
  return(out)
}
