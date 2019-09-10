

#' This function calculates the Word2Vec embeddings
#' @param p_vec A character vector or subset of the FoxCNN corpus where each element is a document
#' @return A vector of predicted match quality scores on a scale of 0-10.
#' @keywords internal
## Loading in function
proc_pretrained_vec <- function(p_vec) {
  # initialize space for values and the names of each word in vocab
  vals <- vector(mode = "list", length(p_vec))
  names <- character(length(p_vec))
  # loop through to gather values and names of each word
  for(i in 1:length(p_vec)) {
    this_vec <- p_vec[i]
    this_vec_unlisted <- unlist(strsplit(this_vec, " "))
    this_vec_values <- as.numeric(this_vec_unlisted[-1])  # this needs testing, does it become numeric?
    this_vec_name <- this_vec_unlisted[1]
    vals[[i]] <- this_vec_values
    names[[i]] <- this_vec_name
  }
  # convert lists to data.frame and attach the names
  glove <- data.frame(vals)
  names(glove) <- names
  return(glove)
}

#' This function calculates the Word2Vec embeddings
#' @import progress
#' @param dat FoxCNN corpus to calculate Word2Vec scores for
#' @return A list of data frames containing the Word2Vec projections of the corpus
#' @export
get_word2vec_glove <- function(dir.source, corpus){
  ## Read in the Fox/CNN data
  tmp = list.files(paste0(getwd(),dir.source))
  names.out = gsub(".txt", "", tmp)
  cat("Reading GloVe word vectors...")
  for (j in 1:length(tmp)){
    glove <- scan(file = paste0(getwd(),dir.source,tmp[j]), what="", sep="\n", quiet=T)
    glove2 <- proc_pretrained_vec(glove)  # this is the actual function call
    glove2 = t(glove2)
    glove2= data.frame(glove2)
    glove2$words = rownames(glove2)
    k = ncol(glove2)
    glove2 = glove2[,c(k, 1:(k-1))]
    temp = softmaxreg::wordEmbed(corpus, dictionary=glove2, meanVec=TRUE)
    assign(names.out[j], temp)
    cat(paste0(j, "out of ", length(tmp), " complete."))
    }
  all=mget(names.out)
  return(all)
}


