#' Refits a STM with a content-based covariate so that all document-level topic-proportions
#' are estimated "as-treated". Also allows for calculation of the SR sufficient reduction and optional coarsening
#' to reduce the dimension of the data, reduce noise, or apply other design rules
#' (e.g. - to exclude words that occur in too few or too many documents).
#'
#' @param mod a fitted \code{\link[stm]{stm}} object
#' @param Z an indicator for treatment assignment
#' @param out the original call to the STM
#' @param calc.SR an indicator for returning the sufficient reduction. Default is TRUE.
#' @param coarsen an indicator for returning the coarsened STM
#' @return A bounded DFM
#' @export


transform_stm = function(mod, out, Z, calc.SR=FALSE, coarsen=FALSE,simplex=FALSE){

  out$meta$Z=1
  meta = out$meta

  mod = as.list(mod)
  
  refit.control = stm::fitNewDocuments(mod, documents=out$documents, newData=out$meta,
                                       origData=meta, prevalence=~Z, betaIndex=~Z, prevalencePrior="None",
                                       verbose=FALSE)


  # Combine estimated thetas for treated with re-fitted thetas for control
  theta2 = mod$theta
  theta2[Z==0,]=refit.control$theta[Z==0,]


  # Calculate sufficient reduction
  if (calc.SR==TRUE){
    k = ncol(mod$theta)
    kappa_c_control = mod$beta$kappa$params[k+1] # (corresponding to control group (Z=0))
    kappa_c_treat = mod$beta$kappa$params[k+2]   # (corresponding to treatment group (Z=1))

    params = mod$beta$kappa$params
    kappa_int_treat = kappa_int_control=c()
    end = length(mod$beta$kappa$params)
    s1 = seq(k+3,end-1,2)
    s2 = seq(k+4,end,2)
    rm(end, k)
    for (j in s1){
      kappa_int_treat = rbind(kappa_int_treat, params[[j]])
    }
    for (j in s2){
      kappa_int_control = rbind(kappa_int_control, params[[j]])
    }
    kc = unlist(kappa_c_control)
    kt = unlist(kappa_c_treat)
    rm(kappa_c_control,kappa_c_treat)

    kic = t(kappa_int_control)
    kit = t(kappa_int_treat)
    rm(kappa_int_control, kappa_int_treat)

    dat = stm::convertCorpus(out$documents, out$vocab, type=c("Matrix"))
    m = rowSums(as.matrix(dat))
    dat2 = t(as.matrix(dat/m))

    PS.noInt= (kc%*%dat2)*(1-Z) + (kt%*%dat2)*Z

    theta = t(as.matrix(mod$theta))
    kappa_theta_c = t(kic%*%theta)
    kappa_theta = t(kit%*%theta)
    kappa_theta[Z==0,]=kappa_theta_c[Z==0,]
    PS.Int = rowSums(kappa_theta*as.matrix(dat))/m

    SR = as.vector(PS.noInt + PS.Int)
    rm(theta, kappa_theta, PS.Int,PS.noInt,dat,dat2,m, kic,kit,kc,kt)
  }


  if (coarsen==TRUE){
    focus = sapply(1:nrow(theta2), function(x) sum(sort(theta2[x,],decreasing=T)[1:3])) # how much of the document is explained by the first 3 topics
    svals = t(as.matrix(apply(theta2,1,order,decreasing=T)))
    svals = as.matrix(svals[,1:3])

    theta3 = theta2
    for (j in 1:nrow(theta3)){
      theta3[j,-c(svals[j,])]=0
    }
    # Renormalize across the topics
    theta3 = theta3/rowSums(theta3)
  }
  if (coarsen==FALSE & calc.SR==TRUE){theta.out=data.frame(cbind(theta2, SR))}
  else if (coarsen==TRUE & calc.SR==FALSE){theta.out =data.frame(theta3, focus)}
  else if (coarsen==TRUE & calc.SR==TRUE){theta.out=data.frame(theta3, focus, SR)}
  else if (coarsen==FALSE & calc.SR==FALSE){theta.out=theta2}
  if(simplex==TRUE){theta.out=theta.out[,-c(1)]} # remove first topic proportion due to collinearity
  theta.out
}
