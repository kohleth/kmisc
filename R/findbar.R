.findBar <- function(term) {
    if (is.name(term) || !is.language(term)) 
        return(NULL)
    if (term[[1]] == as.name("(")) {
        if (term[[2]][[1]] == as.name("|")) {
            return(term)
        } else {
            return(.findBar(term[[2]]))
        }
    }
    stopifnot(is.call(term))
    if (length(term) == 2) 
        return(.findBar(term[[2]]))
    c(.findBar(term[[2]]), .findBar(term[[3]]))
} 
