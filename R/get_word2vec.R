#' This function calculates the Word2Vec embeddings
#'
#' @imports softmaxreg
#' @imports wordVectors
#' @param dat FoxCNN corpus to calculate Word2Vec scores for
#' @return A vector of predicted match quality scores on a scale of 0-10.
#' @export


get_word2vec <- function(dat){
  ## Read in the Fox/CNN data

  g6b_300 <- scan(file = "D:/downloads/word_embeddings/glove.6B.300d.txt", what="", sep="\n")
  glove.300 <- proc_pretrained_vec(g6b_300)  # this is the actual function call
  ## Convert Google's pretrained data to a data.frame
  glove.300 = t(glove.300)
  glove.300 = data.frame(glove.300)
  glove.300$words = rownames(glove.300)
  glove.300 = glove.300[,c(301, 1:300)]
  temp.300 = wordEmbed(dat$Article_Cleaned, dictionary=glove.300, meanVec=TRUE)
  gdata::keep(dat, temp.300, proc_pretrained_vec, sure=T)
  
  g6b_200 <- scan(file = "D:/downloads/word_embeddings/glove.6B.200d.txt", what="", sep="\n")
  glove.200 <- proc_pretrained_vec(g6b_200)  # this is the actual function call
  glove.200 = t(glove.200)
  glove.200 = data.frame(glove.200)
  glove.200$words = rownames(glove.200)
  glove.200 = glove.200[,c(201, 1:200)]
  temp.200 = wordEmbed(dat$Article_Cleaned, dictionary=glove.200, meanVec=TRUE)
  
  gdata::keep(dat, temp.300, temp.200, proc_pretrained_vec, sure=T)
  
  g6b_100 <- scan(file = "D:/downloads/word_embeddings/glove.6B.100d.txt", what="", sep="\n")
  glove.100 <- proc_pretrained_vec(g6b_100)  # this is the actual function call
  glove.100 = t(glove.100)
  glove.100 = data.frame(glove.100)
  glove.100$words = rownames(glove.100)
  glove.100 = glove.100[,c(101, 1:100)]
  temp.100 = wordEmbed(dat$Article_Cleaned, dictionary=glove.100, meanVec=TRUE)
  
  gdata::keep(dat, temp.300, temp.200, temp.100, proc_pretrained_vec, sure=T)
  
  
  g6b_50 <- scan(file = "D:/downloads/word_embeddings/glove.6B.50d.txt", what="", sep="\n")
  glove.50 <- proc_pretrained_vec(g6b_50)  # this is the actual function call
  glove.50 = t(glove.50)
  glove.50 = data.frame(glove.50)
  glove.50$words = rownames(glove.50)
  glove.50 = glove.50[,c(51, 1:50)]
  temp.50 = wordEmbed(dat$Article_Cleaned, dictionary=glove.50, meanVec=TRUE)
  
  
  
  ## Read in the Google pretrained vector
  mod = read.vectors("GoogleNews-vectors-negative300.bin", binary=TRUE)
  
  ## Convert Google's pretrained data to a data.frame
  mod2 = as.data.frame(mod)
  mod2$words = rownames(mod2)
  mod2 = mod2[,c(301, 1:300)]
  
  ## Loop through the articles and score them (meanVec = TRUE)
  temp.300.google = wordEmbed(dat$Article_Cleaned, dictionary=mod2, meanVec=TRUE)
  
  ## Save the output
  
  save(temp.50, temp.100, temp.200, temp.300, temp.300.google, file="Reproduction Files/Data/Word2Vec_output.RData")
  
}



#' This function calculates the Word2Vec embeddings
#'
#' @imports softmaxreg
#' @imports wordVectors
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
    if(i %% 1000 == 0) {print(i)}
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


