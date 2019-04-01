#' Dataset containing distance measurements and average quality scores
#' for sample of 505 pairs of matched Fox and CNN articles evaluated by human
#' coders on Mechanical Turk. Raw quality scores are regressed on distance measurements
#' to fit a predictive model for match quality as a function of the 117 metrics considered.
#'
#' @details Distance measurements and average quality scores for a sample of 505
#' matched pairs of documents evaluated by human coders.
#' @keywords data
#' @format A \link{data.frame} with 505 pairs and 108 features.
#' @references Mozer et al. (2019)
#' \\"Matching with Text Data: An Experimental Evaluation of Methods for Matching Documents
#'  and of Measuring Match Quality\\". \emph{Political Analysis}, Forthcoming.
"FoxCNNsurvey"

#' Dataset containing metadata for FoxCNN corpus with 1,565 articles from CNN and 1,796 articles from Fox News.
#'
#' @details Metadata for the FoxCNN corpus including article names, original URLs, and dates of publication.
#' @keywords data
#' @format A \link{data.frame} with 3,361 observations of 5 articles
#' @references Mozer et al. (2019)
#'  \href{https://reaganmozer.com/textmatching}{Matching with Text Data: An Experimental Evaluation of Methods for Matching Documents
#'  and of Measuring Match Quality}. \emph{Political Analysis}, Forthcoming.
"FoxCNNmeta"


#' Corpus with 1,565 articles from CNN and 1,796 articles from Fox News.
#'
#' @details Corpus of front-page news articles published online by CNN or Fox News
#' from 12/20/2014 to 05/09/2015 containing 1,565 articles from CNN and 1,796 articles from Fox News.
#' Data contains article identifiers corresponding to data in FoxCNNmeta as well as raw and cleaned text data.
#' @keywords data
#' @format A \link{data.frame} with 3,361 observations of 5 articles
#' @references Mozer et al. (2019)
#'  \href{https://reaganmozer.com/textmatching}{Matching with Text Data: An Experimental Evaluation of Methods for Matching Documents
#'  and of Measuring Match Quality}. \emph{Political Analysis}, Forthcoming.
"FoxCNNcorpus"



#' Fitted model for pairwise match quality as a function of 117 distance metrics
#' calculated in Mozer et al. (2019). Trained on "FoxCNNsurvey" dataset.
#'
#' @details Fitted model for predicting the match quality score for a given
#' pair of text documents as a function of 117 distance measurements.
#' @format A \code{\link{glmnet}} model object.
#' @references Mozer et al. (2019)
#' \\"Matching with Text Data: An Experimental Evaluation of Methods for Matching Documents
#'  and of Measuring Match Quality\\". \emph{Political Analysis}, Forthcoming.
"quality_model"
