#' Given a model for match quality and a corpus of documents, calculate the 
#' estimated match quality of each potential pairing of treated and control documents
#' and return a matched dataset containing all pairs of documents with estimated quality
#' above a specified threshold
#'
#' @param corpus description
#' @param Z treatment indicator
#' @param threshold for quality scores to return
#' @return A \link{data.frame} of matched pairs of documents
#' @export
#' 

select_matches = function(corpus, Z, mod, threshold){
  
}