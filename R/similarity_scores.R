#' This function calculates an input character vector's similarity matrix according to the measures contained in the predictive model.
#'
#' @param x A character vector where each element is a document
#' 
#' @return A data frame of rows (n * n-1) and columns 16; each column is one of the constituent similarity measures
#' @export

get_similarity_scores = function(x){
  corp = quanteda::corpus(x)
}