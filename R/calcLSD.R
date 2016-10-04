#' Approximate pairwise least significant interval from an asreml object.
#' 
#' It approximate a pariwise LSD from the sed matrix from an asreml predict call. The goal is to ease plotting.
#' 
#' @param asrpred Result of a \code{predict.asreml(..., sed=TRUE)$prediction} call.
#' @param weight A weight matrix with the same dimension as the \code{sed} matrix. It feeds into the \code{lm} call which weighs the \code{sed} matrix.
#' @param order One of \code{"no"}(default), \code{"increasing"}, or \code{"decreasing"}. Whether the resulting plot should be ordered by the predicted mean.
#' @examples
#' \donttest{
#' data(oats)
#' oats.asr <-
#'   asreml(yield ~ Variety * Nitrogen,
#'          random = ~ Blocks / Wplots,
#'          data = oats)
#' oats.pred <-
#'   predict(oats.asr, classify = "Variety", sed = TRUE)$prediction
#' plotLSD(oats.pred)
#' plotLSD(oats.pred, order = "inc")
#' plotLSD(oats.pred, order = "dec")
#' calcLSD(oats.pred)
#' }




#' @export
#' @rdname calcLSD 
calcLSD=function(asrpred,weight=NULL){
  if(is.null(weight))weight=matrix(1,nrow = nrow(asrpred$sed),ncol=ncol(asrpred$sed))
  W=weight[lower.tri(weight)]
  seds=as.numeric(asrpred$sed[lower.tri(asrpred$sed)])
  di=matrix(1:nrow(asrpred$sed),nrow=nrow(asrpred$sed),ncol=ncol(asrpred$sed),byrow=FALSE)
  di=as.numeric(di[lower.tri(di)])
  dj=matrix(rep(1:nrow(asrpred$sed),each=nrow(asrpred$sed)),nrow=nrow(asrpred$sed),ncol=ncol(asrpred$sed),byrow=FALSE)
  dj=as.numeric(dj[lower.tri(dj)])
  diX=model.matrix(~factor(di)-1)
  djX=model.matrix(~factor(dj)-1)
  X=cbind(djX[,1],djX[,-1]+diX[,-ncol(diX)],diX[,ncol(diX)])
  lm=lm.wfit(X,seds,w=W)
  out=lm$coef
  names(out)=asrpred$pvals[,1]
  
  ## diagnostic
  estsed=outer(out,out,FUN="+")
  diag(estsed)=0
  percentdiff=(estsed-asrpred$sed)/asrpred$sed*100
  attr(out,"(estsed-sed)/sed*100")=percentdiff
  class(out)="lsd"
  return(out)
}


print.lsd=function(x){
  xa=x
  attributes(x)=NULL
  names(x)=names(xa)
  print(x)
}

#' @export
#' @rdname calcLSD 
plotLSD=function(asrpred,weight=NULL,order=c("no","increasing","decreasing"),...){
  require(latticeExtra)
  order=match.arg(order)
  lsd=calcLSD(asrpred,weight)
  LL=asrpred$pvals$predicted.value-2*lsd
  UL=asrpred$pvals$predicted.value+2*lsd
  switch(order,
         "no"=segplot(factor(asrpred$pvals[,1])~LL+UL,centers=asrpred$pvals$predicted.value,draw.bands=FALSE,...),
         "increasing"=segplot(reorder(factor(asrpred$pvals[,1]),asrpred$pvals$predicted.value)~LL+UL,centers=asrpred$pvals$predicted.value,draw.bands=FALSE,...),
         "decreasing"=segplot(reorder(factor(asrpred$pvals[,1]),-asrpred$pvals$predicted.value)~LL+UL,centers=asrpred$pvals$predicted.value,draw.bands=FALSE,...)
         )
}
