print.allCV = function(x) {
    cat("\nCV-loss:\n")
    print.default(format(attr(x, "loss")), quote = FALSE)
    cat("\nCV-predictions:\n")
    print.default(format(x), quote = FALSE)
    invisible(x)
}

summary.allCV = function(x) {
    cat("\nModels:\n")
    comb = getAllComb(formula(attr(x, "models")[[length(attr(x, "models"))]]))
    comb2p = cbind(comb, loss = attr(x, "loss"))
    comb2p = comb2p[order(comb2p[, ncol(comb2p)], decreasing = FALSE), 
        ]
    if (nrow(comb2p) > 10) {
        print.default(head(comb2p, 10), quote = FALSE)
        cat(paste("...", nrow(comb2p) - 10, "more models not shown.\n"))
        
    } else {
        print.default(comb2p, quote = FALSE)
    }
    
    cat("\nlossfn:")
    print.default(attr(x, "lossfn"))
    invisible(x)
}

plot.allCV = function(x) {
    pred = x[, ncol(x)]
    fullmodel = c(tail(attr(x, "models"), n = 1), tail(attr(x, "fixedModels"), 
        n = 1))[[1]]
    obs = model.response(model.frame(formula = fullmodel, data = attr(x, 
        "data")))
    plot(x = obs, y = pred, xlab = "observed", ylab = "predicted", main = "Predicted vs Observed of full model", 
        type = "n")
    points(x = obs, y = pred)
    grid()
    abline(0, 1)
    cat("Press [enter] to continue\n")
    line <- readline()
    plot(sort(attr(x, "loss")), xlab = "model rank", ylab = "loss", main = "Model losses")
    cat("Press [enter] to continue")
    line <- readline()
    comb = getAllComb(fullmodel)
    if (ncol(comb) > 12) 
        warning("There are too many predictors, the variable importance plot might not be too useful.")
    
    comb2p = cbind(comb, loss = attr(x, "loss"))
    ranked = comb2p[order(comb2p[, ncol(comb2p)], decreasing = FALSE), 
        ]
    cs = apply(ranked[, -ncol(ranked)], 2, cumsum)
    cs = cs/(1:nrow(cs))
    matplot(cs, type = "l", lty = 1, lwd = 2, col = RColorBrewer::brewer.pal(ncol(comb), 
        "Paired"), ylab = "running freq.", xlab = "# of models (ranked by MSPE)", 
        main = "Variable Importance")
    identifyMat <- function(x, ...) {
        identify(row(x), x, labels = colnames(cs)[col(x)], ...)
    }
    cat("Click on where the variable label should appear (in graph). Hit [Esc] when finish.")
    y = identifyMat(cs)
} 
