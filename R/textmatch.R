#' This function runs the main ML model as specified in Mozer et al. (2018)
#'
#' @param x A character vector or subset of the FoxCNN corpus where each element is a document
#' @return A vector of predicted match quality scores on a scale of 0-10.

textmatch = function(x, outcome = "matrix"){

  load("qualityModel")

  #scores = get_similarity_scores(x)
  if(outcome == "matrix"){
   # preds = predict_simil_matrix(scores)
  } else if(outcome == "df"){
    #preds = predict_simil_df(scores)
  } else if(outcome == "matches"){
    #preds = predict_simil_matches(scores)
  } else(stop("Outcome must be one of matrix, df, matches"))
  return(preds)
}
