#' Fit a predictive model for pairwise match quality as a function of
#' different distance metrics and text representations, trained on a sample
#' of matched pairs of Fox and CNN articles evaluated by human coders on
#' Mechanical Turk. See Mozer et al. (2019) Section 4 for details of the data
#' collection process.
#'
#' @import glmnet
#' @param x a matrix of pairwise distances for all potential matches of treatment and control units. See \link{pair_distances}.
#' @param Z a vector of treatment indicators
#' @param model.adj an indicator for whether the quality scores reported should be raw estimates or model-adjusted estimates.
#' Defaults to raw estimates of match quality.
#' @return A \link{data.frame} of matched pairs of documents
#' @export
