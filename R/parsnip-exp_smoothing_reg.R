#' General Interface for Exponential Smoothing Models
#'
#' `exponential_smoothing()` is a way to generate a _specification_ of an ETS model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `Rlgt`.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param seasonality This specification of seasonality will be overridden by frequency of y, 
#' if y is of ts or msts class. 1 by default, i.e. no seasonality.
#' @param second_seasonality Second seasonality.
#' @param seasonality_type Either "multiplicative" (default) or "generalized". 
#' The latter seasonality generalizes additive and multiplicative seasonality types.
#' @param method "HW", "seasAvg", "HW_sAvg". Here, "HW" follows Holt-Winters approach. 
#' "seasAvg" calculates level as a smoothed average of the last seasonality number of points 
#' (or seasonality2 of them for the dual seasonality model), and HW_sAvg is an weighted 
#' average of HW and seasAvg methods.
#' @param error_method Function providing size of the error. Either "std" (monotonically, but slower than proportionally, 
#' growing with the series values) or "innov" (proportional to a smoothed abs size of innovations, i.e. surprises)
#'
#'
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `exponential_smoothing()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - "stan" (default) - Connects to [Rlgt::rlgt()]
#'
#' __Main Arguments__
#'
#' The main arguments (tuning parameters) for the model are:
#'
#'  - `seasonality`: Seasonality.
#'  - `second_seasonality`: Second seasonality.
#'  - `seasonality_type`: Either "multiplicative" (default) or "generalized".
#'  - `method`: "HW", "seasAvg", "HW_sAvg"
#'  - `error_method`:  Either "std"  or "innov"
#'
#' These arguments are converted to their specific names at the
#'  time that the model is fit.
#'
#' Other options and argument can be
#'  set using `set_engine()`.
#'
#'  If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#' __stan (default engine)__
#'
#' The engine uses [Rlgt::rlgt()].
#'
#'
#' Parameter Notes:
#' - `xreg` - This is supplied via the parsnip / bayesmodels `fit()` interface
#'  (so don't provide this manually). See Fit Details (below).
#'
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
#'  - Formula Interface: `fit(y ~ date)` will ignore xreg's.
#'
#' __Multivariate (xregs, Exogenous Regressors)__
#'
#'  The `xreg` parameter is populated using the `fit()` function:
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
#'  The `month.lbl` is an exogenous regressor that can be passed to the `expotential_smoothing()` using
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
#' splits <- rsample::initial_time_split(m750, prop = 0.8)
#'
#' # ---- ARIMA ----
#'
#' # Model Spec
#' model_spec <- exponential_smoothing() %>%
#'     set_engine("stan")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date + month(date), data = training(splits))
#' model_fit
#'}
#' @export
exponential_smoothing <- function(mode = "regression", seasonality = NULL, second_seasonality = NULL, seasonality_type = NULL,
                                  method = NULL, error_method = NULL) {
    
    args <- list(
        seasonality        = rlang::enquo(seasonality),
        second_seasonality = rlang::enquo(second_seasonality),
        seasonality_type   = rlang::enquo(seasonality_type),
        method             = rlang::enquo(method),
        error_method       = rlang::enquo(error_method)
    )
    
    parsnip::new_model_spec(
        "exponential_smoothing",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )
    
}

#' @export
print.exponential_smoothing <- function(x, ...) {
    cat("Exponential Smoothing Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)
    
    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }
    
    invisible(x)
}

#' @export
#' @importFrom stats update
update.exponential_smoothing <- function(object, parameters = NULL,
                                         seasonality = NULL, second_seasonality = NULL, seasonality_type = NULL,
                                         method = NULL, error_method = NULL,
                                         fresh = FALSE, ...) {
    
    parsnip::update_dot_check(...)
    
    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }
    
    args <- list(
        seasonality        = rlang::enquo(seasonality),
        second_seasonality = rlang::enquo(second_seasonality),
        seasonality_type   = rlang::enquo(seasonality_type),
        method             = rlang::enquo(method),
        error_method       = rlang::enquo(error_method)
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
        "exponential_smoothing",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.exponential_smoothing <- function(x, engine = x$engine, ...) {
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
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param seasonality Seasonality
#' @param seasonality2 Second seasonality
#' @param seasonality.type 	Either "multiplicative" (default) or "generalized". 
#' The latter seasonality generalizes additive and multiplicative seasonality types.
#' @param level.method 	"HW", "seasAvg", "HW_sAvg"
#' @param error.size.method Either "std" (monotonically, but slower than proportionally, 
#' growing with the series values) or "innov" (proportional to a smoothed abs size of innovations, i.e. surprises)
#' @param ... Additional arguments passed to `forecast::Arima`
#' 
#' @return A modeltime model
#'
#' @export
exp_smoothing_stan_fit_impl <- function(x, y, seasonality = 1, seasonality2 = 1, seasonality.type = "multiplicative", 
                                        error.size.method = "std", level.method = "HW", ...) {
    
    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    outcome    <- y
    predictor  <- x
    
    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- modeltime::parse_index_from_data(predictor)
    period    <- modeltime::parse_period_from_index(index_tbl, "auto")
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)
    
    names_predictor <- names(predictor) %>% dplyr::setdiff(idx_col)
    
    predictor <- predictor %>% dplyr::select(dplyr::all_of(names_predictor))
    
    # XREGS
    # Clean names, get xreg recipe, process predictors
    xreg_recipe <- modeltime::create_xreg_recipe(predictor, prepare = TRUE)
    xreg_matrix <- modeltime::juice_xreg_recipe(xreg_recipe, format = "matrix")
    
    # FIT
    outcome <- stats::ts(outcome, frequency = period)
    
    if (!is.null(xreg_matrix)) {
        fit_smooth   <- Rlgt::rlgt(y                 = outcome,
                                   seasonality       = seasonality,
                                   seasonality2      = seasonality2,
                                   seasonality.type  = seasonality.type,
                                   error.size.method = error.size.method,
                                   level.method      = level.method,
                                   xreg              = xreg_matrix,
                                   ...)
    } else {
        fit_smooth   <- Rlgt::rlgt(y                 = outcome,
                                   seasonality       = seasonality,
                                   seasonality2      = seasonality2,
                                   seasonality.type  = seasonality.type,
                                   error.size.method = error.size.method,
                                   level.method      = level.method,
                                   ...)
    }
    
    rlgt_fit=function(x){
        fit=rstan::extract(x,pars="l")
        fit1=fit$l
        d=dim(fit1)
        values=c()
        for(i in 1:d[2]){
            values[i]=mean(fit1[,i])
        }
        return(values)
    }
    
    rlgt_res=function(x,y){
        res=x-y
        return(res)
    }
    
    # RETURN
    modeltime::new_modeltime_bridge(
        class = "exp_smoothing_stan_fit_impl",
        
        # Models
        models = list(
            model_1 = fit_smooth
        ),
        
        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            !! idx_col  := idx ,
            .actual      =  outcome,
            .fitted      =  rlgt_fit(fit_smooth$samples),
            .residuals   =  rlgt_res(outcome, rlgt_fit(fit_smooth$samples))
        ),
        
        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(
            xreg_recipe = xreg_recipe
        ),
        
        # Description - Convert arima model parameters to short description
        desc = "Exponential Smoothing Model"
    )
    
}

#' @export
print.exp_smoothing_stan_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}



#' @export
predict.exp_smoothing_stan_fit_impl <- function(object, new_data, ...) {
    exp_smoothing_stan_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for ARIMA models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `forecast::Arima()`
#' 
#' @return A prediction
#'
#' @export
exp_smoothing_stan_predict_impl <- function(object, new_data, ...) {
    
    # PREPARE INPUTS
    model       <- object$models$model_1
    
    preds_foecast <- stats::predict(model, new_data, ...)
    
    # Return predictions as numeric vector
    preds <- tibble::as_tibble(preds_forecast) %>% purrr::pluck(1)
    
    return(preds)
    
}

