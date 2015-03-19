#' (Cross) Validate all nested models
#' 
#' It fits all subset regression, allowing interaction and categorical covariates, and (cross) validates them.
#' The code borrows heavily from package \code{meifly}.
#' 
#' @param x Either an \code{lm} object or a two side \code{\link[stats]{formula}}.
#' @param data Data.
#' @param method Currently must be 'lm'
#' @param k Postive integer. k-fold cross validation is carried out. Defaults to 5.
#' @param valid_prob Positive number between 0 and 1. 
#' It is the proportion of data used in the validation set.
#' @param lossfn A function with 2 arguments. The first takes an array of predicted value, the second takes a vector of observed value. 
#'  Then it calculates the loss. 
#'  It defaults to a function which calculates the mean sqaured error (MSE).#'  @param finalfit if \code{TRUE} models are refitted to the entire dataset at the end.
#' @return An array of losses. This array also has a number of attributes:
#' @return \code{models} The fitted models or just formulae of models, depending on \code{finalfit}.
#' @return \code{data} The data used in the fitting.
#' @examples
#' ## Using cvAll
#' \donttest{
#' allreg=cvAll(conc~uptake*Treatment,CO2)
#' allreg
#' attr(allreg,'models') 
#' 
#' calcmae=function(pred,obs)mean(abs(pred-obs),na.rm=TRUE)
#' allreg=cvAll(conc~uptake*Treatment,CO2,lossfn=calcmae)
#' 
#' ## Using cvAll with class lm
#' lm0=lm(conc~uptake*Treatment,CO2)
#' allreg2=cvAll(lm0,CO2)
#' 
#' ## Using validateAll
#' allreg=validateAll(conc~uptake*Treatment,CO2)
#' }
#' @export
cvAll = function(x, data, method, k, ...) UseMethod("cvAll")

#' @export
# @rdname cvAll
cvAll.default<- function(x, data, k, lossfn, 
    ind, finalfit, ...) {
    ## Train the models ---------------------
    pred = plyr::alply(1:k, 1, function(j) {
        train_dat = subset(data, ind != j)
        valid_dat = subset(data, ind == j)
        trained_models = kmisc::fitAll(x = x, data = train_dat, progress = FALSE)
        cbind(id = which(ind == j), sapply(trained_models, predict, newdata = valid_dat))
    })
    pred = do.call(rbind, pred)
    pred = pred[order(pred[, "id"]), -1]
    loss = apply(pred, 2, lossfn, obs = model.frame(formula(x), data = data, 
        na.action = NULL)[, 1])
    if (finalfit == TRUE) {
        ## Fitting the models -------------------------
        models <- fitAll(x = x, data = data)
        names(models) <- seq_along(models)
        return(structure(loss, models = models))
    } else {
        forms = getAllForm(x, data,class=class(x))
        return(structure(loss, models = forms))
    }
    
}

#' @export
# @rdname cvAll
cvAll.lm = function(x, data, k = 5, lossfn = .calcMSE, 
                    ind=NULL, finalfit = TRUE, ...){
  ## Splliting out a validation set
  if (is.null(ind)) 
    ind = createSCV(form = formula(x), data = data, k = k)
  cvAll.default(x=x,data=data,k=k,ind=ind,lossfn=lossfn,finalfit=finalfit,...)
  
  }


cvAll.lme = function(x, data, k = 5, lossfn = .calcMSE, 
                     ind=NULL, finalfit = TRUE, ...) {
  ## Splliting out a validation set
  if (is.null(ind)){
    fixed=all.vars(x$call$fixed)
    random=all.vars(x$call$random)
    ind = createSCV(form =  reformulate(c(fixed,random)), data = data, k = k)
  } 

  cvAll.default(x=x,data=data,k=k,ind=ind,lossfn=lossfn,finalfit=finalfit,...)
  
}

cvAll.merMod =  function(x, data, k = 5, lossfn = .calcMSE, 
                         ind=NULL, finalfit = TRUE, ...) {
  ## Splliting out a validation set
  if (is.null(ind)) 
    ind = createSCV(form = x, data = data, k = k)
  cvAll.default(x=x,data=data,k=k,ind=ind,lossfn=lossfn,finalfit=finalfit,...)
  
}
 
.calcMSE = function(pred, obs) mean((pred - obs)^2, na.rm = TRUE)
