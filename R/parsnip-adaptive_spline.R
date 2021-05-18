#' General Interface for Adaptive Spline Surface Models
#'
#' `adaptive_spline()` is a way to generate a _specification_ of an Adaptive Spline Surface model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `BASS`.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param splines_degree degree of splines. Stability should be examined for anything other than 1.
#' @param max_degree integer for maximum degree of interaction in spline basis functions. 
#' Defaults to the number of predictors, which could result in overfitting.
#' @param max_categorical_degree (categorical input only) integer for maximum degree of interaction of categorical inputs.
#' @param min_basis_points minimum number of non-zero points in a basis function. If the response is functional, this refers only to the portion of the basis function coming from the non-functional predictors. 
#' Defaults to 20 or 0.1 times the number of observations, whichever is smaller.
#'
#'
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `adaptive_spline()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - "stan" (default) - Connects to [BASS::bass()]
#'
#' __Main Arguments__
#'
#' The main arguments (tuning parameters) for the model are:
#'
#'  - `splines_degree`
#'  - `max_degree`
#'  - `max_categorical_degree`
#'  - `min_basis_points`
#'
#' These arguments are converted to their specific names at the
#'  time that the model is fit.
#'
#' Other options and argument can be
#'  set using `set_engine()` (See Engine Details below).
#'
#'  If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#'
#' @section Engine Details:
#'
#' Other options can be set using `set_engine()`.
#'
#' __stan (default engine)__
#'
#' The engine uses [BASS::bass()].
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
#' This algorithm only accepts multivariate: you need to pass xregs (read next section).
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
#'  The `month.lbl` is an exogenous regressor that can be passed to the `sarima_reg()` using
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
#' library(lubridate)
#'
#' # Data
#' m750 <- m4_monthly %>% filter(id == "M750")
#' m750
#'
#' # Split Data 80/20
#' splits <- rsample::initial_time_split(m750, prop = 0.8)
#'
#' # ---- Adaptive Spline ----
#'
#' # Model Spec
#' model_spec <- adaptive_spline() %>%
#'     set_engine("stan")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date + month(date), data = training(splits))
#' model_fit
#'}
#' @export
adaptive_spline <- function(mode = "regression", 
                            splines_degree = NULL, 
                            max_degree = NULL, 
                            max_categorical_degree = NULL, 
                            min_basis_points = NULL) {
    
    args <- list(
        splines_degree         = rlang::enquo(splines_degree),
        max_degree             = rlang::enquo(max_degree),
        max_categorical_degree = rlang::enquo(max_categorical_degree),
        min_basis_points       = rlang::enquo(min_basis_points)
    )
    
    parsnip::new_model_spec(
        "adaptive_spline",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )
    
}

#' @export
print.adaptive_spline <- function(x, ...) {
    cat("Adaptive Spline Surface Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)
    
    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }
    
    invisible(x)
}

#' @export
#' @importFrom stats update
update.adaptive_spline <- function(object, 
                                   parameters = NULL,
                                   splines_degree = NULL, 
                                   max_degree = NULL, 
                                   max_categorical_degree = NULL, 
                                   min_basis_points = NULL,
                                   fresh = FALSE, ...) {
    
    parsnip::update_dot_check(...)
    
    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }
    
    args <- list(
        splines_degree         = rlang::enquo(splines_degree),
        max_degree             = rlang::enquo(max_degree),
        max_categorical_degree = rlang::enquo(max_categorical_degree),
        min_basis_points       = rlang::enquo(min_basis_points)
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
        "adaptive_spline",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.adaptive_spline <- function(x, engine = x$engine, ...) {
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
#' @param degree degree of splines
#' @param maxInt integer for maximum degree of interaction in spline basis functions
#' @param maxInt.cat (categorical input only) integer for maximum degree of interaction of categorical inputs
#' @param npart minimum number of non-zero points in a basis function
#' @param ... Extra arguments
#' 
#' @return A modeltime model
#'
#' @export
adaptive_spline_stan_fit_impl <- function(x, y, degree = 1, maxInt = 3, maxInt.cat = 3, npart = NULL, ...) {
    
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
    #outcome <- stats::ts(outcome, frequency = period)
    
    if (!is.null(xreg_matrix)) {
        
        fit_bass <- BASS::bass(xx         = xreg_matrix,
                               y          = outcome,
                               maxInt     = maxInt,
                               maxInt.cat = maxInt.cat,
                               degree     = degree,
                               npart      = npart,
                               ...
                               )
        
    } else {
        rlang::abort("External Regressors must be passed to this algorithm")
    }

        
    
    # RETURN
    modeltime::new_modeltime_bridge(
        class = "adaptive_spline_stan_fit_impl",
        
        # Models
        models = list(
            model_1 = fit_bass
        ),
        
        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            !! idx_col  := idx,
            .actual      =  outcome,
            .fitted      =  fit_bass$yhat.mean,
            .residuals   =  .actual - .fitted
        ),
        
        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(
            xreg_recipe = xreg_recipe,
            predictors   = names(predictor)
        ),
        
        # Description - Convert arima model parameters to short description
        desc = "Adaptive Spline Surface Model"
    )
    
}

#' @export
print.adaptive_spline_stan_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}



#' @export
predict.adaptive_spline_stan_fit_impl <- function(object, new_data, ...) {
    adaptive_spline_stan_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for ARIMA models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `forecast::Arima()`
#' 
#' @return A prediction
#'
#' @export
adaptive_spline_stan_predict_impl <- function(object, new_data, ...) {
    
    # PREPARE INPUTS
    model       <- object$models$model_1
    xreg_recipe <- object$extras$xreg_recipe
    predictors  <- object$extras$predictors
    
    new_data <- new_data %>% dplyr::select(dplyr::all_of(predictors))
    
    # XREG
    xreg_matrix <- modeltime::bake_xreg_recipe(xreg_recipe, new_data, format = "matrix")
    
    preds_forecast <- stats::predict(model, newdata = xreg_matrix, ...)
    
    preds <- vector(length = dim(preds_forecast)[2])
    
    for(j in 1:dim(preds_forecast)[2]){
        preds[j] <- mean(preds_forecast[,j], na.rm = TRUE)
    }
    
    return(preds)
    
}




