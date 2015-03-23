#' Cross Validate all nested models
#' 
#' It fits all subset regression, allowing interaction and categorical covariates, and cross validates them.
#' The code borrows heavily from package \code{meifly}.
#' 
#' @param x Either an \code{lm} object or a two side \code{\link[stats]{formula}}.
#' @param data Data.
#' @param k Postive integer. k-fold cross validation is carried out. Defaults to 5.
#' @param valid_prob Positive number between 0 and 1. 
#' It is the proportion of data used in the validation set.
#' @param lossfn A function with 2 arguments. The first takes an array of predicted value, the second takes a vector of observed value. 
#'  Then it calculates the loss. 
#'  It defaults to a function which calculates the mean sqaured error (MSE).  
#' @param finalfit if \code{TRUE} models are refitted to the entire dataset at the end.
#' @return An maxtrix of predictions on the validation set, with the following attributes:
#' @return \code{loss} The loss calculated by \code{lossfn}.
#' @return \code{data} The data used.
#' @return \code{models} The fitted models or just formulae of models, depending on \code{finalfit}.
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
#' }
#' @export
cvAll = function(x, ...) UseMethod("cvAll")

#' @export
#' @rdname cvAll
cvAll.default <- function(x, data, k, lossfn = .calcMSE, ind, finalfit = TRUE) {
    ## Train the models ---------------------
    pred = plyr::alply(1:k, 1, function(j) {
        train_dat = subset(data, ind != j)
        valid_dat = subset(data, ind == j)
        trained_models = kmisc::fitAll(x = x, data = train_dat, progress = FALSE)
        cbind(id = which(ind == j), sapply(trained_models, predict, newdata = valid_dat,allow.new.levels=TRUE))
    })
    pred = do.call(rbind, pred)
    pred = pred[order(pred[, "id"]), -1]
    if (inherits(x, "merMod")) {
        formX = nobars(formula(x))
    } else {
        formX = formula(x)
    }
    dimnames(pred) = list(1:nrow(pred), models = 1:ncol(pred))
    loss = apply(pred, 2, lossfn, obs = model.frame(formX, data = data, 
        na.action = NULL)[, 1])
    out = structure(pred, loss = loss, data = data, lossfn = substitute(lossfn), 
        k = k, ind = ind, class = "allCV")
    
    if (finalfit == TRUE) {
        ## Fitting the models -------------------------
        models <- fitAll(x = x, data = data)
        names(models) <- seq_along(models)
        attr(out, "models") = models
    } else {
        attr(out, "fixedModels") = getAllForm(formX)
        
    }
    return(out)
}

#' @export
#' @rdname cvAll
cvAll.lm = function(x, data, k = 5, ind = NULL, ...) {
    ## Splliting out a validation set
    if (is.null(ind)) 
        ind = createSCV(form = formula(x), data = data, k = k)
    cvAll.default(x = x, data = data, k = k, ind = ind, ...)
    
}

#' @export
#' @rdname cvAll
cvAll.lme = function(x, data, k = 5, ind = NULL, ...) {
    ## Splliting out a validation set
    if (is.null(ind)) {
        fixed = all.vars(x$call$fixed)
        random = all.vars(x$call$random)
        ind = createSCV(form = reformulate(c(fixed, random)), data = data, 
            k = k)
    }
    cvAll.default(x = x, data = data, k = k, ind = ind, ...)
}

#' @export
#' @rdname cvAll
cvAll.merMod = function(x, data, k = 5, ind = NULL, ...) {
    ## Splliting out a validation set
    if (is.null(ind)) 
        ind = createSCV(form = x, data = data, k = k)
    cvAll.default(x = x, data = data, k = k, ind = ind, ...)
}

.calcMSE = function(pred, obs) mean((pred - obs)^2, na.rm = TRUE)
.calcMAE = function(pred, obs) mean(abs(pred - obs), na.rm = TRUE) 
