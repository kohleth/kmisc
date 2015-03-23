#' Exract coefficients from ensemble object.
#' 
#' It exract coefficients from ensemble object.The code borrows heavily from package \code{meifly}.
#' 
#' @param object Output from \code{fitAll}.
#' @return A data.frame with coefficients.
#' @examples
#' \donttest{
#' allreg=fitAll(conc~uptake*Treatment,CO2)
#' coef(allreg)
#' }

#' @export
coef.allFit = function(object) {
    coefs <- plyr::ldply(object, .coef_simple)
    names(coefs)[1] <- "model"
    coefs$model <- factor(coefs$model)
    all <- expand.grid(model = unique(coefs$model), variable = unique(coefs$variable))
    coefs <- plyr::join(all, coefs, by = c("model", "variable"))
    coefs[is.na(coefs)] <- 0
    rownames(coefs) <- paste("m", coefs$model, "v", as.numeric(coefs$variable), 
        sep = "")
    class(coefs) <- c("variable_ensemble", class(coefs))
    coefs
}


.coef_simple = function(model) {
    trunc <- function(x, trunc) ifelse(abs(x) > trunc, sign(x) * trunc, 
        x)
    coefs <- data.frame(names(coef(model))[-1], summary(model)$coefficients[-1, 
        c(1, 3), drop = FALSE])
    names(coefs) <- c("variable", "raw", "t")
    transform(coefs, t = trunc(t, 3), abst = abs(trunc(t, 3)))
} 
