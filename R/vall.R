#' @export
#' @rdname cvAll
validateAll = function(x, data, method, valid_prob, ...) UseMethod("validateAll")

#' @export
# @rdname cvAll
validateAll.default <- function(x, data, method = "lm", valid_prob = 0.2, 
    lossfn = .calcMSE, finalfit = TRUE, ...) {
    ## Splliting out a validation set
    ind = createSV(form = x, data = data, valid_prob = valid_prob)
    train_dat = subset(data, ind == 1)
    valid_dat = subset(data, ind == 2)
    ## Train the models ---------------------
    trained_models = fitAll(x = x, data = train_dat, description = "Training")
    ## Validate the models -------------------
    message("validating ", length(trained_models), " models...")
    pred = plyr::laply(trained_models, predict, newdata = valid_dat, .progress = "text")
    loss = apply(pred, 1, lossfn, obs = model.frame(formula(x), data = valid_dat, 
        na.action = NULL)[, 1])
    # do.call(lossfn,list(pred=pred,obs=model.frame(formula(x), data =
    # valid_dat,na.action=NULL)[, 1]))
    if (finalfit == TRUE) {
        ## Fitting the models -------------------------
        models <- fitAll(x = x, data = data)
        names(models) <- seq_along(models)
        return(structure(loss, models = models))
    } else {
        forms = lapply(trained_models, formula)
        return(structure(loss, models = forms, data = data))
    }
    
}

#' @export
# @rdname cvAll
validateAll.lm = function(x, data, method = "lm", valid_prob = 0.2, ...) validateAll.default(x = formula(x), 
    data = data, method = method, ...)

 
