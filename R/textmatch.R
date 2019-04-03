#' This function runs the main ML model as specified in Mozer et al. (2018)
#'
#' @param x A character vector or subset of the FoxCNN corpus where each element is a document
#' @return A vector of predicted match quality scores on a scale of 0-10.

textmatch = function(x, outcome = "matrix", verbose=TRUE){
  
  data(FoxCNNcorpus)
  vocab.train = colnames(dfm(FoxCNNcorpus$Text_Cleaned,stem=T, remove=tm::stopwords("en")))
  
  test = tm::stripWhitespace(tm::removePunctuation(x))
  vocab.test = colnames(dfm(test, stem=T, remove=tm::stopwords("en")))
  overlap = intersect(vocab.test, vocab.train)
  stopifnot(length(overlap)>0)
  if (verbose==TRUE){
    print("Vocabulary of text has ", length(overlap), " terms in common with the training data", sep="")
  }
  
  dat = dfm(test, stem=T, remove=tm::stopwords("en"))
  dat = dat[,colnames(dat)%in%overlap]
  
  data("quality.model.RData")
  

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
