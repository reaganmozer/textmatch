#' Corpus contrasts
#' @param corp a fitted \code{\link[stm]{stm}} object
#' @param Z an indicator for treatment assignment
#' @return Corpus contrast summaries
#' @export
#' 

corp_contrast <- function(corp, Z){
  
  v0 = colnames(tm::DocumentTermMatrix(corp[Z==0]))
  v1 = colnames(tm::DocumentTermMatrix(corp[Z==1]))
  rm.terms = setdiff(v0,v1)
  dtm = tm::DocumentTermMatrix(corp)
  all.terms = colnames(dtm)
  common.terms = length(all.terms)-length(rm.terms)
  v0.unique=length(which(!v0%in%v1))
  v1.unique=length(which(!v1%in%v0))
  cat(paste(sprintf("Vocabulary size: %d terms \n", length(all.terms)),
            sprintf("Overlapping terms: %d terms", common.terms),sep=""))
  
  cat(paste(sprintf("Group %s: %s/%s unique terms (%s", unique(Z)[1], v0.unique, length(v0), 
                    round(100*v0.unique/length(v0),0)), "%)",sep=""))
  cat(paste(sprintf("Group %s: %s/%s unique terms (%s", unique(Z)[2], v1.unique, length(v1), 
                    round(100*v1.unique/length(v1),0)), "%)",sep=""))
  
  
  d0 = quanteda::dfm(quanteda::corpus(corp[Z==0]))
  d1 = quanteda::dfm(quanteda::corpus(corp[Z==1]))
  ld0 = mean(1/quanteda::textstat_lexdiv(d0, "TTR")$TTR)
  ld1 = mean(1/quanteda::textstat_lexdiv(d1, "TTR")$TTR)
  }
