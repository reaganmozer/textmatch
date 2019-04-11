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
#' @inheritParams calculate_pair_distances
#' @param form Should the distances be returned as a list of matrices or condensed into a single data frame?
#' @return A matrix showing pairwise distances for all potential matches of treatment and control units under various distance metrics
#' @export


pair_distances = function(dat, Z, propensity.method, all.counts=NULL,
                          include=c("cosine", "euclidean","mahalanobis","propensity"), 
                          exclude="jaccard",
                          form="data.frame", verbose=FALSE){
  
  if(is.null(form)){form="data.frame"}
  stopifnot(form%in%c("data.frame","list"))
  dat.orig=dat
  
  if (!is.dfm(dat)) {
    dat=as.dfm(dat)
  }
  docnames = 1:nrow(as.matrix(dat))
  
  if (is.null(propensity.method)){
    if (is.dfm(dat.orig)){
      propensity.method="mnir"
    }
    else if (!is.dfm(dat.orig)){
      propensity.method="glm"
    }
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
  
  if (form=="data.frame"){
  tmp=as.data.frame(matrix(0, nrow=length(ind), ncol=length(ind2)))
  rownames(tmp)=docnames[ind]
  colnames(tmp)=docnames[ind2]
  tmp = subset(data.table::melt(as.matrix(tmp)),select=c(Var2,Var1))
  names(tmp)=group.names
  tmp$index.0 = as.numeric(tmp$index.0)
  tmp$index.1 = as.numeric(tmp$index.1)
  } else if (form=="list"){
  tmp=list()
  }
  #stopifnot(!is.null(x) & !is.null(Z))
  # Make sure all documents have some features to compare

  
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
      d1 = quanteda::textstat_simil_old(dat, selection=c(ind2),method = calc[j], margin = "documents")
      dist = 1-as.matrix(d1)[c(ind),] #change to distance
      rm(d1)
    } else if (calc[j]=="propensity"){
      if (propensity.method == "mnir"){
        fitIR = textir::mnlm(cl=NULL, covars=as.matrix(Z,ncol=1), counts=dat)
        SR = textir::srproj(fitIR, counts=dat)
        dist = match_on(Z~SR, data=data.frame(Z))
        rm(SR, fitIR, dat3, fwd, counts.fit)
        }
      else if (propensity.method == "glm"){
        fwd = glm(Z~as.matrix(dat.orig), family="binomial")
        dat3=data.frame(ps=fwd$fitted.values)
        dist = as.matrix(Rfast::Dist(dat3))[ind,ind2]
        rm(fwd,dat3)
      }
    } else if (calc[j]=="mahalanobis"){
      dat2 = Rfast::standardise(as.matrix(dat))
      dist = as.matrix(Rfast::Dist(dat2))[ind,ind2]
      rm(dat2)
    }
    else if (calc[j] %in%dists){
      d1 = quanteda::textstat_dist_old(dat, selection=c(ind2),method = calc[j], margin = "documents")
      dist = as.matrix(d1)[c(ind),]
      rm(d1)
    }
    name = paste(calc[j], ".dist", sep="")
    dist = abs(dist)
    if (form=="data.frame"){
    tmp0 = subset(data.table::melt(dist,value.name=name),select=-c(Var2,Var1))
    tmp = cbind(tmp, tmp0)
    rm(tmp0)
    }
    else if (form=="list"){
      tmp = list.append(tmp,list(name=dist))
    }
    rm(name, dist)
  }
  return(tmp)
  rm(dat.orig)
}
