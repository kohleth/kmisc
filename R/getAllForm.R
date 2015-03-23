#' Get All nested model formulae/combination matrix.
#' 
#' getAllForm find all nested models' formulae.
#' getAll FixedComb represent all nested models in terms of a combination matrix.
#' 
#' The nested models are formed under one rule: Interaction terms are present only if main effects are.
#' @param x A formula.
#' @return \code{getAllForm} returns a list of formulae. 
#' @return \code{getAllComb} returns of variable combination matrix.

#' @export
getAllForm = function(x) {
    fixed = lme4::nobars(x)
    combs = getAllComb(fixed)
    vars <- apply(combs, 1, function(i) colnames(combs)[i])
    form <- paste(deparse(fixed[[2]]), "~", lapply(vars, paste, collapse = " + "), 
        sep = "")
    form[[1]] <- paste(deparse(fixed[[2]]), "~1")
    if (!is.null(.findBar(x))) 
        form <- paste(form, .findBar(x), sep = "+")
#     form <- lapply(form, as.formula, env = baseenv())
    form <- lapply(form, as.formula)

#     environment(form)=environment(x)
    form
}

#' @export
#' @rdname getAllForm
getAllComb = function(x) {
    tt <- attr(terms(x), "factors")
    if (ncol(tt) <= 1) 
        stop("formula should be more complicated which allows non-trivial nested models.")
    comb0 <- do.call(expand.grid, rep(list(c(FALSE, TRUE)), ncol(tt)))
    ttf <- tt[-1, ]
    cond <- rbind(ttf, cbind(matrix(0, nrow = diff(dim(ttf)), ncol = nrow(ttf)), 
        diag(diff(dim(ttf)))))
    ncond <- colSums(cond)
    combs <- unique(t(apply(as.matrix(comb0) %*% cond, 1, function(x) x == 
        ncond)))
    combs
} 
