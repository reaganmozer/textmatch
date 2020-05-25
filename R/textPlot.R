#' Plot differences between groups of documents on psychological dimensions measured by LIWC.
#' 
#' @importFrom plotrix plotCI
#' @importFrom scales alpha
#' @param x matrix of LIWC features
#' @param Z vector of treatment indicators.
#' @return a \pkg{plotrix} plot.
#' @export
#' 

textPlot = function(x,Z, sig.only=FALSE, p.adj=NULL, alpha=0.05,
                     main=NULL, test.stat=NULL){
  
  stopifnot(test.stat%in%c("mean","rank",NULL))
  if (is.null(test.stat)){test.stat="mean"}
  
  Z = as.factor(Z)
  if(is.null(main) & !is.null(p.adj)){
    main=paste0("Significant differences after ", toupper(p.adj)," correction")
  }
  else if (is.null(main)){main=""}
  
  if (!sig.only){
    LIWC.plot.all(x,Z, main=main,alpha=alpha)
  }
  else if (test.stat=="rank"){
    LIWC.plot.rs(x,Z,main,alpha=alpha,p.adj=p.adj)
  }
  else if (test.stat=="mean"){
    LIWC.plot.norm(x,Z, main=main,alpha=alpha)
  }
}
  
  
  

LIWC.plot.rs = function(x,Z, p.adj, alpha, main){
  if (is.null(p.adj)){p.adj="none"}
  out = do.call(rbind, lapply(x,function(c)rs_stats(c,Z)))
  out$p.adj = p.adjust(out$p.val, method=p.adj)
  out$CI.width=out$UL-out$LL
  
  out1 = out[out$p.adj<=alpha, ]

  ord = rep(3,nrow(out1))
  ord[out1$name%in%c("Analytic","Authentic","Clout","Tone")]=1
  ord[out1$name%in%c("WC","WPS","TTR","XXX","Sixltr","spellcheck")]=2
  
  out1 = out1[with(out1,order(dplyr::desc(ord),dplyr::desc(rownames(out1)))),]
  cols= ifelse(out1$est>0,"blue","red")
  xl=min(out1$LL)-1
  xr=max(out1$UL)+1
  labs=ifelse(p.adj=="none","Difference in medians","Difference in medians\n(Corrected)")
  plotrix::plotCI(x=out1$est, y=1:nrow(out1), ui=out1$UL, li=out1$LL,
         err="x",yaxt="n",ylab="",xlab=labs,
         main=main, pch=NA,scol="white",xlim=c(xl+0.5,xr-0.5))
  abline(v=0,lty=2)
  
  if (length(unique(ord))==3){
    ind=c(min(which(ord==1)),min(which(ord==2)))-0.5
    rect(xleft=xl,ybottom=ind[2], xright=xr,ytop=ind[1],col=alpha("lightgray",0.05),border=NA)
    rect(xleft=xl,ybottom=0, xright=xr,ytop=ind[2],col=alpha("slategray",0.03),border=NA)
    abline(h=ind[1],col="darkgray",lty=3,lwd=2)
    abline(h=ind[2],col="darkgray",lty=3,lwd=2)
  }
  else if (length(unique(ord)==2)){
    ind = which.min(ord)
    rect(xleft=xl,ybottom=0, xright=xr,ytop=ind,col=alpha("slategray",0.03),border=NA)
    abline(h=ind,col="darkgray",lty=3,lwd=2)
  } 
  
  plotrix::plotCI(x=out1$est, y=1:nrow(out1), ui=out1$UL, li=out1$LL,
         err="x",add=T,
         pch=21, pt.bg=alpha(cols,0.5), cex=1.3,lwd=1.25)
  
  axis(side=2,at=1:nrow(out1),labels=rownames(out1),las=2,cex=0.5,tcl=-0.25)
  
}


LIWC.plot.norm = function(x,Z, alpha, main){
  
  out = norm_stats(x,Z, alpha)
  out$CI.width=out$UL-out$LL
  is.sig = sign(out$UL)==sign(out$LL)
  
  
  if (sum(is.sig)>0){
    out1 = out[is.sig,]
    
    ord = rep(3,nrow(out1))
    ord[rownames(out1)%in%c("Analytic","Authentic","Clout","Tone")]=1
    ord[rownames(out1)%in%c("WC","WPS","TTR","XXX","Sixltr","spellcheck","Flesch.Kincaid")]=2
    
    out1$ord=ord
    out1 = out1[with(out1,order(dplyr::desc(ord), dplyr::desc(rownames(out1)))),]
    cols= ifelse(out1$est>0,"blue","red")
    xl=min(out1$LL)-1
    xr=max(out1$UL)+1
    plotrix::plotCI(x=out1$est, y=1:nrow(out1), ui=out1$UL, li=out1$LL,
           err="x",yaxt="n",ylab="",xlab="Difference in means\n(Corrected)",
           main=main, pch=NA,scol="white",xlim=c(xl+0.5,xr-0.5))
    abline(v=0,lty=2)
    
    if (length(unique(ord))==3){
      ind=c(min(which(out1$ord==1)),min(which(out1$ord==2)))-0.5
      rect(xleft=xl-0.5,ybottom=ind[2], xright=xr+0.5,ytop=ind[1],col=alpha("lightgray",0.05),border=NA)
      rect(xleft=xl-0.5,ybottom=0, xright=xr+0.5,ytop=ind[2],col=alpha("slategray",0.03),border=NA)
      abline(h=ind[1],col="darkgray",lty=3,lwd=2)
      abline(h=ind[2],col="darkgray",lty=3,lwd=2)
    }
    else if (length(unique(ord)==2)){
      ind = which.min(ord)
      rect(xleft=xl,ybottom=0, xright=xr,ytop=ind,col=alpha("slategray",0.03),border=NA)
      abline(h=ind,col="darkgray",lty=3,lwd=2)
    } 
    
    plotrix::plotCI(x=out1$est, y=1:nrow(out1), ui=out1$UL, li=out1$LL,
           err="x",add=T,
           pch=21, pt.bg=alpha(cols,0.5), cex=1.3,lwd=1.25)
    
    which.sigs = sign(out1$UL)==sign(out1$LL)
    axis(side=2,at=1:nrow(out1),labels=rownames(out1),las=2,cex=0.5,tcl=-0.25)
    
  }
  else if (sum(is.sig)==0){
    plotrix::plotCI(x=1:nrow(out), y=1:nrow(out), ui=out$UL, li=out$LL,
           err="x",yaxt="n",ylab="",xlab="Difference in means\n(Corrected)",
           main=main, pch=NA,scol="white")
    text(x=-1+nrow(out)/2, y=nrow(out)/2, "No significant\ndifferences",col="red",cex=0.75)
  }
}



LIWC.plot.all = function(x,Z, alpha, main){
  if (!is.factor(Z)){Z=as.factor(Z)}
  out = norm_stats(x,Z,alpha=alpha)
  out$CI.width=out$UL-out$LL
  out1=out
  
  ord = rep(3,nrow(out1))
  ord[rownames(out1)%in%c("Analytic","Authentic","Clout","Tone")]=1
  ord[rownames(out1)%in%c("WC","WPS","TTR","XXX","Sixltr","spellcheck","Flesch.Kincaid")]=2
  
  out1$ord=ord
  out1 = out1[with(out1,order(dplyr::desc(ord), dplyr::desc(rownames(out1)))),]
  cols= ifelse(out1$est>0,"blue","red")
  xl=min(out1$LL)-1
  xr=max(out1$UL)+1
  plotrix::plotCI(x=out1$est, y=1:nrow(out1),ui=out1$UL, li=out1$LL,
         err="x",yaxt="n",ylab="",xlab="Difference in means\n(Corrected)",
         main=main, pch=NA,scol="white",xlim=c(xl+0.5,xr-0.5))
  abline(v=0,lty=2)
  ind=c(min(which(out1$ord==1)),min(which(out1$ord==2)))-0.5
  rect(xleft=xl-0.5,ybottom=ind[2], xright=xr+0.5,ytop=ind[1],col=alpha("lightgray",0.05),border=NA)
  rect(xleft=xl-0.5,ybottom=0, xright=xr+0.5,ytop=ind[2],col=alpha("slategray",0.03),border=NA)
  abline(h=ind[1],col="darkgray",lty=3,lwd=2)
  abline(h=ind[2],col="darkgray",lty=3,lwd=2)
  
  
  plotrix::plotCI(x=out1$est, y=1:nrow(out1), ui=out1$UL, li=out1$LL,
         err="x",add=T,
         pch=21, pt.bg=alpha(cols,0.5), cex=1.3,lwd=1.25)
  
  which.sigs = sign(out1$UL)==sign(out1$LL)
  y1 = (1:nrow(out1))[!which.sigs]
  y2 = (1:nrow(out1))[which.sigs]
  axis(side=2,at=y1,labels=rownames(out1)[!which.sigs],las=2,cex.axis=0.7,tcl=-0.25)
  axis(side=2,at=y2,labels=rownames(out1)[which.sigs],las=2,cex.axis=0.75,tcl=-0.25,
       font=2)
  
}

#' Calculate differences in medians and perform simultaneous rank-sum tests.
#' 
#' @param x matrix of LIWC features
#' @param Z vector of treatment indicators.
#' @return a \pkg{data.frame} with results.
#' @export
#' 
rs_stats = function(x, Z){
  Z = relevel(Z,"1")
  w=wilcox.test(x~Z, conf.int=T)
  LL = w$conf.int[1]
  UL = w$conf.int[2]
  est= w$estimate
  p.val=w$p.value
  out = data.frame(est, p.val, LL, UL)
  rownames(out)=NULL
  out
}


#' Calculate differences in means and calculate simultaneous confidence intervals
#' with automatic adjustment for multiple comparisons.
#' 
#' @param x matrix of LIWC features
#' @param Z vector of treatment indicators.
#' @return a \pkg{data.frame} with results.
#' @export
#' 
norm_stats = function(x,Z,alpha){
  require(SimComp)
  if (!is.factor(Z)){Z=as.factor(Z)}
  dat = cbind(x,Z)
  s=SimComp::SimCiDiff(dat,"Z",covar.equal=T,
              conf.level=1-alpha)
  out = data.frame(est=as.vector(s$estimate), LL.raw=as.vector(s$lower.raw),
                   UL.raw=as.vector(s$upper.raw), LL = as.vector(s$lower), UL=as.vector(s$upper))
  out$var=s$resp
  out = dplyr::select(out, var, est, everything())
  rownames(out)=s$resp
  out
}
