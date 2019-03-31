#' This function runs the main ML model as specified in Mozer et al. (2018)
#'
#' @param x A character vector where each element is a document
#'
#' @return An n by n matrix where n is the length of parameter x. Each entry is a standardized similarity score.
#' @export
#' @examples
#' textmatch(c("I am a dog", "I am a cat", "The rain in Spain falls mainly on the plain."),
#'  outcome = "matrix")
#'

textmatch = function(x, outcome = "matrix"){
  scores = get_similarity_scores(x)
  if(outcome == "matrix"){
    preds = predict_simil_matrix(scores)
  } else if(outcome == "df"){
    preds = predict_simil_df(scores)
  } else if(outcome == "matches"){
    preds = predict_simil_matches(scores)
  } else(stop("Outcome must be one of matrix, df, matches"))
  return(preds)
}
