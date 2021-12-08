#' General Interface for Bayesian Structural Time Series Models
#'
#' `bayesian_structural_reg()` is a way to generate a _specification_ of a Bayesian Structural Time Series Model 
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `bsts`.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param distribution The model family for the observation equation. 
#' Non-Gaussian model families use data augmentation to recover a conditionally Gaussian model.
#'
#'
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `bayesian_structural_reg()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - "stan" (default) - Connects to [bsts::bsts()]
#'
#' __Main Arguments__
#'
#' Other options and argument can be
#'  set using `set_engine()` (See Engine Details below).
#'
#'  If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#' __stan (default engine)__
#'
#' The engine uses [bsts::bsts()].
#'
#' Parameter Notes:
#' - `xreg` - This is supplied via the parsnip / modeltime `fit()` interface
#'  (so don't provide this manually). See Fit Details (below).
#'
#' @section Fit Details:
#'
#' __Date and Date-Time Variable__
#'
#' It's a requirement to have a date or date-time variable as a predictor.
#' The `fit()` interface accepts date and date-time features and handles them internally.
#'
#' - `fit(y ~ date)`
#'
#' __Univariate (No xregs, Exogenous Regressors):__
#'
#' For univariate analysis, you must include a date or date-time feature. Simply use:
#'
#'  - Formula Interface (recommended): `fit(y ~ date)` will ignore xreg's.
#'
#' __Multivariate (xregs, Exogenous Regressors)__
#'
#'  The `xreg` parameter is populated using the `fit()` or `fit_xy()` function:
#'
#'  - Only `factor`, `ordered factor`, and `numeric` data will be used as xregs.
#'  - Date and Date-time variables are not used as xregs
#'  - `character` data should be converted to factor.
#'
#'  _Xreg Example:_ Suppose you have 3 features:
#'
#'  1. `y` (target)
#'  2. `date` (time stamp),
#'  3. `month.lbl` (labeled month as a ordered factor).
#'
#'  The `month.lbl` is an exogenous regressor that can be passed to the `arima_reg()` using
#'  `fit()`:
#'
#'  - `fit(y ~ date + month.lbl)` will pass `month.lbl` on as an exogenous regressor.
#'
#'  Note that date or date-time class values are excluded from `xreg`.
#'
#'
#'
#' @seealso [fit.model_spec()], [set_engine()]
#' 
#' @return A model spec
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(parsnip)
#' library(rsample)
#' library(timetk)
#' library(modeltime)
#' library(bayesmodels)
#'
#' # Data
#' m750 <- m4_monthly %>% filter(id == "M750")
#' m750
#'
#' # Split Data 80/20
#' splits <- initial_time_split(m750, prop = 0.8)
#' 
#' ss <- AddLocalLinearTrend(list(), training(splits)$value)
#'
#' # Model Spec
#' model_spec <- bayesian_structural_reg() %>%
#'     set_engine("stan", state.specification = ss)
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#' model_fit
#' 
#' predict(model_fit, testing(splits))
#'
#'}
#' @export
bayesian_structural_reg <- function(mode = "regression", distribution = NULL) {
    
    args <- list(
        distribution = rlang::enquo(distribution)
    )
    
    parsnip::new_model_spec(
        "bayesian_structural_reg",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )
    
}

#' @export
print.bayesian_structural_reg <- function(x, ...) {
    cat("Bayesian Structural Regression Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)
    
    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }
    
    invisible(x)
}

#' @export
#' @importFrom stats update
update.bayesian_structural_reg <- function(object, parameters = NULL,
                           distribution = NULL,
                           fresh = FALSE, ...) {
    
    parsnip::update_dot_check(...)
    
    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }
    
    args <- list(
        distribution = rlang::enquo(distribution)
    )
    
    args <- parsnip::update_main_parameters(args, parameters)
    
    if (fresh) {
        object$args <- args
    } else {
        null_args <- purrr::map_lgl(args, parsnip::null_value)
        if (any(null_args))
            args <- args[!null_args]
        if (length(args) > 0)
            object$args[names(args)] <- args
    }
    
    parsnip::new_model_spec(
        "bayesian_structural_reg",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.bayesian_structural_reg <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'stan'` for translation.")
        engine <- "stan"
    }
    x <- parsnip::translate.default(x, engine, ...)
    
    x
}


# FIT - Arima -----

#' Low-Level ARIMA function for translating modeltime to forecast
#'
#' @param data A dataframe of xreg (exogenous regressors)
#' @param formula A numeric vector of values to fit
#' @param family The model family for the observation equation. 
#' Non-Gaussian model families use data augmentation to recover a conditionally Gaussian model.
#' @param ... Additional arguments passed to `forecast::Arima`
#' 
#' @return A modeltime model
#'
#' @export
bayesian_structural_stan_fit_impl <- function(data, formula, family = "gaussian",  ...) {
    
    args <- list(...)
    
    if (!any(names(args) %in% "state.specification")) {
        
        rlang::abort("Please, provide the state.specification argument trhough set_engine.")
        
    }
    
    if (!any(names(args) %in% "niter")) {
        
        args[["niter"]] <- 1000
        
    }
    
    y <- all.vars(formula)[1]
    x <- attr(stats::terms(formula, data = data), "term.labels")
    
    predictors <- data %>% dplyr::select(dplyr::all_of(x))
    
    # INDEX 
    # Determine Index Col
    #index_tbl <- modeltime::parse_index_from_data(predictors)
    #idx_col   <- names(index_tbl)
    
    index_tbl <- parse_index_from_data(predictors)
    period    <- parse_period_from_index(index_tbl, "auto")
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)
    
    x <- dplyr::setdiff(x, idx_col)
    
    if (length(x)==0){
        formula <- as.vector(purrr::as_vector(data[, y]))
        var <- y
    } else {
        formula <- stats::reformulate(termlabels = x, response = y) 
        var <- c(x, y)
    }
    
    args[["formula"]] <- formula
    args[["data"]] <- data
    
    model_call <-parsnip::make_call(fun  = "bsts",
                                    ns   = "bsts",
                                    args = args)
    
    model_fit <- rlang::eval_tidy(model_call)
    
    # RETURN
    modeltime::new_modeltime_bridge(
        class = "bayesian_structural_stan_fit_impl",
        
        # Models
        models = list(
            model_1 = model_fit
        ),
        
        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            #.date        = timetk::tk_make_timeseries(start_date = "1970", length_out = length(model_fit$original.series)),
            !! idx_col  := idx,
            .actual      =  as.numeric(model_fit$original.series),
            .fitted      =  .actual - as.numeric(apply(residuals(model_fit), 2, mean)),
            .residuals   =  as.numeric(apply(residuals(model_fit), 2, mean))
        ),
        
        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(
            date_var = idx_col,
            predictors = var
        ),
        
        # Description - Convert arima model parameters to short description
        desc = "Bayesian Structural Model"
    )
    
}

#' @export
print.bayesian_structural_stan_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}



#' @export
predict.bayesian_structural_stan_fit_impl <- function(object, new_data, ...) {
    bayesian_structural_stan_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for ARIMA models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `forecast::Arima()`
#' 
#' @return A prediction
#'
#' @export
bayesian_structural_stan_predict_impl <- function(object, new_data, ...) {
    
    # PREPARE INPUTS
    model       <- object$models$model_1
    date_var    <- object$extras$date_var
    predictors  <- object$extras$predictors
    
    old_data <- new_data %>% dplyr::select(-date_var) %>% dplyr::select(predictors)
    
    if (ncol(old_data) == 1){
        comp <- apply(old_data, 1, is.na) %>% sum()
    } else {
        comp <- apply(old_data, 1, is.na) %>% apply(., 1, sum)
    }
    
    if (all(comp==dim(old_data)[1]) | ncol(old_data) == 1){
        preds <- stats::predict(model, h = nrow(new_data), ...)$mean
    } else {
        preds <- stats::predict(model, newdata = new_data, ...)$mean
    }
    
    return(preds)
    
}

