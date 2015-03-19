


#' Create Stratified (Cross-)Validation Set
#' 
#' It creates stratified (cross-)validation folds from data according to formula, so that all levels of all factors in \code{formula} are as balancely scattered as possible. 
#' This avoids problem with simple un-stratified cross validation where a factor level presents only in the validation set but not in the training set.
#' 
#' @param form A \code{formula}. The LHS of \code{form} (if exists) and all non-factors in the RHS will be removed before forming strata.
#' @param data Data.
#' @param k number of folds to form.
#' @param valid_prob Percentage of data to appear in validation set. Only used in \code{createSV}.
#' @return A numeric vector with entries indicating the fold it belongs to.
#' @examples
#' \donttest{
#' createSCV(~Type+Treatment,CO2)
#' createSV(~Type+Treatment,CO2)
#' 
#' set.seed(123)
#' f1=createSCV(~Type+Treatment,CO2)
#' set.seed(123)
#' f2=createSCV(conc~Type+Treatment+uptake,CO2)
#' identical(f1,f2) ## TRUE
#' }

#' @describeIn createSCV
createSCV = function(form, data, k = 5) {
    form = .getrhsfactor(form, data)
    id = 1:nrow(data)
    alloc = numeric(nrow(data))
    formu = as.formula(paste("id", paste0(as.character(form), collapse = ""), collapse = ""))
    dump = aggregate(formu, data = data, function(x, k) {
        if (length(x) >= k) {
            alloc[x] <<- sample(rep(1:k, length.out = length(x)))
        } else {
            alloc[x] <<- sample(1:k, length(x))
        }
    }, k = k)
    return(alloc)
}

#' @describeIn createSCV
createSV = function(form, data, valid_prob = 0.2) {
    if (valid_prob < 0 | valid_prob > 1) 
        stop("valid_prob must be between 0 and 1")
    form = .getrhsfactor(form, data)
    id = 1:nrow(data)
    alloc = numeric(nrow(data))
    formu = as.formula(paste("id", paste0(as.character(form), collapse = ""), collapse = ""))
    dump = aggregate(formu, data = data, function(x) {
        if (length(x) <= 2) {
            alloc[x] <<- 1:length(x)
        } else {
            first = rep(1:2, length(x) * c(1 - valid_prob, valid_prob))
            if (length(first) != length(x)) 
                first = c(1, first)
            alloc[x] <<- sample(first)
        }
    })
    return(alloc)
}

.getrhsfactor = function(form, data) {
    form = formula(delete.response(terms(form, data = data)))  ## delete LHS
    vartype = lapply(data.frame(data[all.vars(form)]), class)  ## work out variable class
    vars = all.vars(form)[sapply(vartype,function(x)"factor"%in%x)]  ## remove numeric variables
    formula(paste0("~", paste(vars, collapse = "+")))  ## form formula
} 
