#' Similarity and distance computation between documents or features
#'
#' These functions compute distance matrices from a text representation where each row is a document
#' and each column is a feature to measure distance over based on treatment indicator Z
#'
#' @import data.table
#' @import quanteda
#' @param x a matrix text representation with rows corresponding to each document in a corpus and columns
#' that represent summary measures of the text (e.g., word counts, topic proportions, etc.). Acceptable forms include
#' a valid \pkg{quanteda} \code{dfm} object, a \pkg{tm} Document-Term Matrix, or a matrix of estimated topic proportions.
#' @param Z A logical or binary vector indicating treatment and control for each unit in the study.
#' TRUE or 1 represents a treatment unit, FALSE of 0 represents a control unit.
#' @param propensity.method Either GLM or MNIR for propensity score estimation
#' @param docnames A vector of document names equal in length to the number of documents
#' @param form Should the distances be returned as a list of matrices or condensed into a single data frame?
#' @return A matrix showing pairwise distances for all potential matches of treatment and control units under various distance metrics
#' @export


pair_distances = function(dat, Z, form=c("data.frame", "list"),
                              include=c("cosine", "euclidean","mahalanobis","propensity"),
                              propensity.method=c("glm","mnir"),
                              exclude= "jaccard", docnames=NULL,
                              verbose=TRUE){

  if (!is.dfm(dat)) {
    dat.orig=dat
    dat=as.dfm(dat)
  }

  group.names = c("index.0", "index.1")
  if (length(unique(Z))!=2){
    stop("treatment indicator Z must be binary")
  } else if (is.numeric(Z)){
    group.names = paste("index.",unique(Z),sep="")
    Z = as.logical(Z)
  } else if (is.character(Z)){
    group.names = paste("index.",unique(Z),sep="")
    Z = as.logical(Z==unique(Z)[1])
  }

  ind = which(Z==TRUE)
  ind2 = which(Z==FALSE)

  tmp=as.data.frame(matrix(0, nrow=length(ind), ncol=length(ind2)))
  rownames(tmp)=rownames(dat)[ind]
  colnames(tmp)=rownames(dat)[ind2]
  tmp = subset(data.table::melt(as.matrix(tmp)),select=c(Var1,Var2))
  names(tmp)=group.names
  tmp$index.0 = as.numeric(tmp$index.0)
  tmp$index.1 = as.numeric(tmp$index.1)

  #stopifnot(!is.null(x) & !is.null(Z))

  if (!is.null(docnames)){
    stopifnot(length(docnames)==length(Z))
    rownames(dat)=docnames
  } else if (is.null(docnames)){
    rownames(dat)=1:nrow(dat)
  }

  # Calculate each distance

  # Similarity metrics
  simil = c("cosine","jaccard","correlation", "ejaccard", "dice", "edice",
            "hamman", "simple matching", "faith")

  # Distance metrics
  dists = c("euclidean","kullback",
            "manhattan", "maximum", "canberra","minkowski")

  # Distance metrics not included in quanteda
  oth = c("mahalanobis","propensity")
  all.methods = c(simil, dists,oth)

  calc = all.methods[all.methods%in%include & !all.methods%in%exclude]
  for (j in 1:length(calc)){
    if (verbose==TRUE){
      print(paste("Calculating ", calc[j], " distances...",sep=""))
    }

    if (calc[j]%in%simil){
      d1 = quanteda::textstat_simil(dat, selection=c(ind2),method = calc[j], margin = "documents")
      dist = 1-as.matrix(d1)[c(ind),] #change to distance
      rm(d1)
    } else if (calc[j]=="propensity"){
      if (propensity.method == "mnir"){
        fitCS = textir::mnlm(NULL, as.matrix(Z,ncol=1), dat)
        SR = textir::srproj(fitCS, dat)
        fwd=glm(Z~SR,family="binomial")
        dat3=data.frame(ps=fwd$fitted.values)
        dist = as.matrix(Rfast::Dist(dat3))[ind,ind2]
        rm(SR, fitCS)
        }
      else if (propensity.method == "glm"){
        fwd = glm(Z~as.matrix(dat.orig), family="binomial")
        dat3=data.frame(ps=fwd$fitted.values)
        dist = as.matrix(Rfast::Dist(dat3))[ind,ind2]
      }
        rm(fwd,dat3)
    } else if (calc[j]=="mahalanobis"){
      dat2 = Rfast::standardise(as.matrix(dat))
      dist = as.matrix(Rfast::Dist(dat2))[ind,ind2]
      rm(dat2)
    }
    else if (calc[j] %in%dists){
      d1 = quanteda::textstat_dist(dat, selection=c(ind2),method = calc[j], margin = "documents")
      dist = as.matrix(d1)[c(ind),]
      rm(d1)
    }
    name = paste(calc[j], ".dist", sep="")
    tmp0 = subset(data.table::melt(dist,value.name=name),select=-c(Var1,Var2))
    tmp = cbind(tmp, tmp0)
    rm(tmp0, name, dist)
  }
  rm(dat.orig)
  tmp
}
