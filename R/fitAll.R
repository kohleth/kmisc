#' All subset regression
#' 
#' It fits all subset regression, allowing interaction and categorical covariates. The code borrows heavily from package \code{meifly}.
#' 
#' @param x A model \code{object} of one of the classes supported.
#' @param data Data.
#' @return A list with n components. n is the number of models fitted.
#' @examples
#' ## Using fitAll with formula class
#' \donttest{
#' lm0=lm(conc~uptake*Treatment,CO2)
#' allreg=fitAll(lm0,CO2)
#' }

#' @export
fitAll <- function(x, data,...) UseMethod("fitAll")


#' @export
#' @rdname fitAll 
fitAll.lm <- function(x, data, description = "fitting", progress = TRUE, 
    ...) {
    form = formula(x)
    method = class(x)
    forms = getAllForm(form, class=class(x), ...)
    if (progress == TRUE) 
        message(description, " ", length(forms), " models...")
    method <- as.name(method)
    fitmodel <- function(f) {
        eval(substitute(method(f, data = data, ...), list(f = f, method = method)))
    }
    models <- plyr::llply(forms, fitmodel, .progress = ifelse(progress == 
        TRUE, "text", "none"))
    names(models) <- seq_along(models)
    structure(models, class = "allFit")
}

#' @export
#' @rdname fitAll 
fitAll.lme <- function(x, data, description = "fitting", 
    progress = TRUE, ...) {
    fixed = formula(x)
    random = as.formula(x$call$random)
    method = class(x)
    forms = getAllForm(fixed, class=class(x), ...)
    if (progress == TRUE) 
        message(description, " ", length(forms), " models...")
    method <- as.name(method)
    fitmodel <- function(f) {
        eval(substitute(method(f, data = data, random = random, ...), list(f = f, 
            method = method)))
    }
    models <- plyr::llply(forms, fitmodel, .progress = ifelse(progress == 
        TRUE, "text", "none"))
    names(models) <- seq_along(models)
    structure(models, class = "allFit")
}


#' @export
#' @rdname fitAll 
fitAll.merMod <- function(x, data, description = "fitting", progress = TRUE, 
    ...) {
    method = "lmer"
    forms = getAllForm(formula(x), ...)
    if (progress == TRUE) 
        message(description, " ", length(forms), " models...")
    method <- as.name(method)
    fitmodel <- function(f) {
        eval(substitute(method(f, data = data, ...), list(f = f, method = method)))
    }
    models <- plyr::llply(forms, fitmodel, .progress = ifelse(progress == 
        TRUE, "text", "none"))
    names(models) <- seq_along(models)
    structure(models, class = "allFit")
}


