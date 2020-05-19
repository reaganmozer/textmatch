#' Extract an array of numerical features from a collection of text documents
#' 
#' @param text a character vector of text documents
#' @param control a named list of control options for evaluating documents individually and globally
#' @return a \code{data.frame} representation of text with columns corresponding to text-based features
#' @export


tada <- function(text, lower=TRUE, alphaNum=TRUE, stopwords=NULL, stem=FALSE){
  
  stopifnot(is.character(text))
  
  
  if (alphaNum){
    text = iconv(text, from="UTF-8", to="ASCII", sub="XX")
    text = gsub("[^[:alnum:][:blank:]+.,'?!:;&/\\-]", "", text)
  }
  if (lower){
    text = tolower(text)
  }
  if (!is.null(stopwords)){
    text = tm::removeWords(text,tm::stopwords)
  }
  
  text
  
  
}
  