#' Constructor for creating garchmodels models
#'
#' These functions are used to construct new `garchmodels` bridge functions that
#' connect the `tidymodels` infrastructure to time-series models containing date or date-time features.
#'
#' @param class A class name that is used for creating custom printing messages
#' @param models A list containing one or more models
#' @param data A data frame (or tibble) containing 4 columns:
#'  (date column with name that matches input data), .actual, .fitted, and .residuals.
#' @param extras An optional list that is typically used for transferring preprocessing recipes
#'  to the predict method.
#' @param desc An optional model description to appear when printing your modeltime objects

#' @export
#' @return A list with the constructor
new_modelgarch_bridge <- function(class, models, data, extras = NULL, desc = NULL) {
    
    if (missing(class)) rlang::abort("'class' must be a character vector. This is used to define a print method.")
    if (!is.character(class)) rlang::abort("'class' must be a character vector. This is used to define a print method.")
    
    msg <- "'models' should be a list:\n 1. The first model should named 'model_1'.\n 2. Subsequent models should be named 'model_2' and so on."
    if (missing(models)) rlang::abort(paste0("'models' is missing.\n\n", msg))
    if (!is.list(models)) rlang::abort(paste0("'models' is not a list().\n\n", msg))
    if (!all(stringr::str_detect(names(models), pattern = "^model_"))) rlang::abort(paste0("'model' has bad list names. Try naming 'model_1'.\n\n", msg))
    
    msg <- "'extras' should be a list. It's often used for adding preprocessing recipes."
    if (!is.null(extras)) {
        if (!is.list(extras)) rlang::abort(msg)
    }
    
    msg <- "'desc' should be a single character value. It's often used for printing a description of your model using a print method."
    if (!is.null(desc)) {
        if (!is.character(desc)) rlang::abort(paste0("'desc' is not of class character.\n", msg))
        if (length(desc) != 1) rlang::abort(paste0("'desc' length is not 1.\n", msg))
    }
    
    # CONSTRUCTOR
    ret <- list(
        models = models,
        extras = extras,
        desc   = desc
    )
    
    class(ret) <- c(class, "modelgarch_bridge")
    
    return(ret)
    
}

#' @export
print.modelgarch_bridge <- function(x, ...) {
    if (!is.null(x$desc)) cat(paste0(x$desc, "\n"))
    print(x$models)
    invisible(x)
}


#' Delete Dimension Attribute
#'
#' This function is a internal helper.
#'
#'@param x Data.frame

#' @export
#' @return A tibble
delete_attr <- function(x){
    
    attr(x, "dimnames") <- NULL
    
    return(x)
    
}