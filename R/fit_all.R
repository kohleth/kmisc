#' All subset regression
#' 
#' It fits all subset regression, allowing interaction and categorical covariates. The code borrows heavily from package \code{meifly}.
#' 
#' @param x Either an \code{lm} object or a two side \code{\link[stats]{formula}}.
#' @param data Data.
#' @param method Currently must be 'lm'
#' @return A list with n components. n is the number of models fitted.
#' The list also has an attribute:
#' @return \code{data} The data used in the fitting.
#' @examples
#' ## Using fitAll with formula class
#' \donttest{
#' allreg=fitAll(conc~uptake*Treatment,CO2)
#' allreg[[1]]
#' attr(allreg,'data') 
#' 
#' ## Using fitAll with lm class
#' lm0=lm(conc~uptake*Treatment,CO2)
#' allreg2=fitAll(lm0,CO2)
#' }

#' @export
fitAll <- function(x, data, method, ...) UseMethod("fitAll")

#' @export
#' @rdname fitAll 
fitAll.default <- function(x, data, method = "lm", description = "fitting", progress = TRUE, 
    ...) {
    form = .getAllForm(x, data,method=method,...)
    if (progress == TRUE) 
        message(description, " ", length(form), " models...")
    method <- as.name(method)
    fitmodel <- function(f) {
        eval(substitute(method(f, data = data, ...), list(f = f, method = method)))
    }
    models <- plyr::llply(form, fitmodel, .progress = ifelse(progress == TRUE, "text", 
        "none"))
    names(models) <- seq_along(models)
    structure(models, data = data, class = "ensemble")
}

#' @export
#' @rdname fitAll 
fitAll.lm <- function(x, data, method = "lm", ...) {
    x = formula(x)
    fitAll.default(x, data, method, ...)
}

#' @export
#' @rdname fitAll 
fitAll.lme <- function(x,data,method = "lme",...){
  fixed=formula(x)
  random=as.formula(x$call$random)
  fitAll.default(x=fixed,data,method,random=random)
}


#' @export
#' @rdname fitAll 
fitAll.lmer <- function(x,data,method = "lmer",...){
  fixed=nobars(formula(x))
  random=paste0("(",fb2(formula(x)),")")
  fitAll.default(x=fixed,data,method,random=random)
}

.getAllForm = function(x, data, method,random=NULL,...) {
    tt <- attr(terms(x, data = data), "factors")
    if (ncol(tt) <= 1) 
        stop("formula should be more complicated which allows non-trivial nested models.")
    comb0 <- do.call(expand.grid, rep(list(c(FALSE, TRUE)), ncol(tt)))
    ttf <- tt[-1, ]
    cond <- rbind(ttf, cbind(matrix(0, nrow = diff(dim(ttf)), ncol = nrow(ttf)), diag(diff(dim(ttf)))))  ## conditions for interaction term to exist
    ncond <- colSums(cond)
    combs <- unique(t(apply(as.matrix(comb0) %*% cond, 1, function(x) x == ncond)))
    vars <- apply(combs, 1, function(i) colnames(combs)[i])
    form <- paste(deparse(x[[2]]), "~", lapply(vars, paste, collapse = " + "), sep = "")
    form[[1]] <- paste(deparse(x[[2]]), "~1")
    if(method=="lmer")form<-paste(form,random,sep="+")
    form <- lapply(form, as.formula, env = baseenv())
    form
} 

fb2 <- function(term) {
  if (is.name(term) || !is.language(term)) 
    return(NULL)
  if (term[[1]] == as.name("(")) 
    return(fb2(term[[2]]))
  stopifnot(is.call(term))
  if (term[[1]] == as.name("|")) 
    return(term)
  if (length(term) == 2) 
    return(fb2(term[[2]]))
  c(fb2(term[[2]]), fb2(term[[3]]))
}

getAllFormMat=function (x, data) 
{
  tt <- attr(terms(x, data = data), "factors")
  if (ncol(tt) <= 1) 
    stop("formula should be more complicated which allows non-trivial nested models.")
  comb0 <- do.call(expand.grid, rep(list(c(FALSE, TRUE)), ncol(tt)))
  ttf <- tt[-1, ]
  cond <- rbind(ttf, cbind(matrix(0, nrow = diff(dim(ttf)), 
                                  ncol = nrow(ttf)), diag(diff(dim(ttf)))))
  ncond <- colSums(cond)
  combs <- unique(t(apply(as.matrix(comb0) %*% cond, 1, function(x) x == 
                            ncond)))
  combs
}