#' General Interface for Additive Linear State Space Regression Models
#'
#' `additive_state_space()` is a way to generate a _specification_ of a Additive Linear State Space Regression Model 
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `bayesforecast`.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param trend_model a boolean value to specify a trend local level model. By default is FALSE.
#' @param damped_model a boolean value to specify a damped trend local level model. By default is FALSE.
#' @param seasonal_model a boolean value to specify a seasonal local level model. By default is FALSE.
#' @param seasonal_period an integer specifying the periodicity of the time series by default the value frequency(ts) is used
#' @param garch_t_student a boolean value to specify for a generalized t-student SSM model.
#' @param markov_chains An integer of the number of Markov Chains chains to be run, by default 4 chains are run.
#' @param chain_iter An integer of total iterations per chain including the warm-up, by default the number of iterations are 2000.
#' @param warmup_iter A positive integer specifying number of warm-up (aka burn-in) iterations. This also specifies the number of iterations used for step-size adaptation, so warm-up samples should not be used for inference. The number of warmup should not be larger than iter and the default is iter/2.
#' @param adapt_delta An optional real value between 0 and 1, the thin of the jumps in a HMC method. By default is 0.9
#' @param tree_depth An integer of the maximum depth of the trees evaluated during each iteration. By default is 10.
#' @param pred_seed An integer with the seed for using when predicting with the model.
#'
#'
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `additive_state_space()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - "stan" (default) - Connects to [bayesforecast::stan_ssm()]
#'
#' __Main Arguments__
#'
#' The main arguments (tuning parameters) for the model are:
#'
#'  - `trend_model`: a boolean value to specify a trend local level model. By default is FALSE.
#'  - `damped_model`: a boolean value to specify a damped trend local level model. By default is FALSE.
#'  - `seasonal_model`: a boolean value to specify a seasonal local level model. By default is FALSE.
#'  - `markov_chains`: An integer of the number of Markov Chains chains to be run.
#'  - `adapt_delta`: The thin of the jumps in a HMC method.
#'  - `tree_depth`: The maximum depth of the trees evaluated during each iteration.
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
#' The standardized parameter names in `bayesmodels` can be mapped to their original
#' names in each engine:
#'
#' ```{r echo = FALSE}
#' tibble::tribble(
#'    ~"bayesmodels", ~"bayesforecast::stan_ssm",
#'    "trend_model", "trend", 
#'    "damped_model", "damped",
#'    "seasonal_model", "seasonal",
#'    "seasonal_period", "period",
#'    "markov_chains", "chains(4)",
#'    "adapt_delta", "adapt.delta(0.9)",
#'    "tree_depth", "tree.depth(10)"
#' ) %>% knitr::kable()
#' ```
#'
#' Other options can be set using `set_engine()`.
#'
#' __stan (default engine)__
#'
#' The engine uses [bayesforecast::stan_ssm()].
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
#' _Seasonal Period Specification_
#'
#' The period can be non-seasonal (`seasonal_period = 1 or "none"`) or
#' yearly seasonal (e.g. For monthly time stamps, `seasonal_period = 12`, `seasonal_period = "12 months"`, or `seasonal_period = "yearly"`).
#' There are 3 ways to specify:
#'
#' 1. `seasonal_period = "auto"`: A seasonal period is selected based on the periodicity of the data (e.g. 12 if monthly)
#' 2. `seasonal_period = 12`: A numeric frequency. For example, 12 is common for monthly data
#' 3. `seasonal_period = "1 year"`: A time-based phrase. For example, "1 year" would convert to 12 for monthly data.
#'
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
#' splits <- rsample::initial_time_split(m750, prop = 0.8)
#'
#' # ---- AUTO ARIMA ----
#'
#' # Model Spec
#' model_spec <- additive_state_space() %>%
#'     set_engine("stan")
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
additive_state_space <- function(mode = "regression", trend_model = NULL, damped_model = NULL, seasonal_model = NULL, 
                    seasonal_period = NULL, garch_t_student = NULL, markov_chains = NULL,
                    chain_iter = NULL, warmup_iter = NULL, adapt_delta = NULL, tree_depth = NULL, pred_seed = NULL) {
    
    args <- list(
        trend_model               = rlang::enquo(trend_model),
        damped_model              = rlang::enquo(damped_model),
        seasonal_model            = rlang::enquo(seasonal_model),
        seasonal_period           = rlang::enquo(seasonal_period),
        garch_t_student           = rlang::enquo(garch_t_student),
        
        markov_chains             = rlang::enquo(markov_chains),
        chain_iter                = rlang::enquo(chain_iter),
        warmup_iter               = rlang::enquo(warmup_iter),
        adapt_delta               = rlang::enquo(adapt_delta),
        tree_depth                = rlang::enquo(tree_depth),
        
        pred_seed                 = rlang::enquo(pred_seed)
    )
    
    parsnip::new_model_spec(
        "additive_state_space",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )
    
}

#' @export
print.additive_state_space <- function(x, ...) {
    cat("SSM Regression Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)
    
    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }
    
    invisible(x)
}

#' @export
#' @importFrom stats update
update.additive_state_space <- function(object, parameters = NULL,
                           trend_model = NULL, damped_model = NULL, seasonal_model = NULL, seasonal_period = NULL, 
                           markov_chains = NULL, garch_t_student = NULL,
                           chain_iter = NULL, warmup_iter = NULL, adapt_delta = NULL, tree_depth = NULL, pred_seed = NULL,
                           fresh = FALSE, ...) {
    
    parsnip::update_dot_check(...)
    
    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }
    
    args <- list(
        trend_model               = rlang::enquo(trend_model),
        damped_model              = rlang::enquo(damped_model),
        seasonal_model            = rlang::enquo(seasonal_model),
        seasonal_period           = rlang::enquo(seasonal_period),
        garch_t_student           = rlang::enquo(garch_t_student),
        
        markov_chains             = rlang::enquo(markov_chains),
        chain_iter                = rlang::enquo(chain_iter),
        warmup_iter               = rlang::enquo(warmup_iter),
        adapt_delta               = rlang::enquo(adapt_delta),
        tree_depth                = rlang::enquo(tree_depth),
        
        pred_seed                 = rlang::enquo(pred_seed)
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
        "additive_state_space",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.additive_state_space <- function(x, engine = x$engine, ...) {
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
#' @param trend a boolean value to specify a trend local level model. By default is FALSE.
#' @param damped a boolean value to specify a damped trend local level model. By default is FALSE.
#' @param seasonal a boolean value to specify a seasonal local level model.
#' @param period an integer specifying the periodicity of the time series.
#' @param genT a boolean value to specify for a generalized t-student SSM model.
#' @param chains An integer of the number of Markov Chains chains to be run, by default 4 chains are run.
#' @param iter An integer of total iterations per chain including the warm-up, by default the number of iterations are 2000.
#' @param warmup A positive integer specifying number of warm-up (aka burn-in) iterations. This also specifies the number of iterations used for step-size adaptation, so warm-up samples should not be used for inference. The number of warmup should not be larger than iter and the default is iter/2.
#' @param adapt.delta An optional real value between 0 and 1, the thin of the jumps in a HMC method. By default is 0.9
#' @param tree.depth An integer of the maximum depth of the trees evaluated during each iteration. By default is 10.
#' @param seed An integer with the seed for using when predicting with the model.
#' @param ... Additional arguments passed to `forecast::Arima`
#' 
#' @return A modeltime model
#'
#' @export
ssm_stan_fit_impl <- function(x, y, trend = FALSE, damped = FALSE, seasonal = FALSE, period = 0, genT = FALSE,
                              chains = 4, iter = 2000, warmup = iter/2, 
                              adapt.delta = 0.9, tree.depth = 10, seed = NULL, ...) {
    
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
    
    # XREGS
    # Clean names, get xreg recipe, process predictors
    xreg_recipe <- modeltime::create_xreg_recipe(predictor, prepare = TRUE)
    xreg_matrix <- modeltime::juice_xreg_recipe(xreg_recipe, format = "matrix")
    
    # FIT
    outcome <- stats::ts(outcome, frequency = period)
    
    if (!is.null(xreg_matrix)) {
        fit_ssm   <- bayesforecast::stan_ssm(outcome, 
                                             trend         = trend,
                                             damped        = damped,  
                                             xreg          = xreg_matrix,
                                             period        = period,
                                             genT          = genT,
                                             chains        = chains,
                                             iter          = iter,
                                             warmup        = warmup,
                                             tree.depth    = tree.depth,
                                             adapt.delta   = adapt.delta,
                                             ...)
    } else {
        fit_ssm   <- bayesforecast::stan_ssm(outcome, 
                                             trend         = trend,
                                             damped        = damped,  
                                             period        = period,
                                             genT          = genT,
                                             chains        = chains,
                                             iter          = iter,
                                             warmup        = warmup,
                                             tree.depth    = tree.depth,
                                             adapt.delta   = adapt.delta,
                                             ...)
    }
    
    # RETURN
    modeltime::new_modeltime_bridge(
        class = "ssm_stan_fit_impl",
        
        # Models
        models = list(
            model_1 = fit_ssm
        ),
        
        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            !! idx_col  := idx,
            .actual      =  as.numeric(fit_ssm$model$yreal),
            .fitted      =  .actual - as.numeric(residuals(fit_ssm)),
            .residuals   =  as.numeric(residuals(fit_ssm))
        ),
        
        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(
            xreg_recipe = xreg_recipe,
            pred_seed   = seed
        ),
        
        # Description - Convert arima model parameters to short description
        desc = "Additive Linear State Space Model"
    )
    
}

#' @export
print.ssm_stan_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}



#' @export
predict.ssm_stan_fit_impl <- function(object, new_data, ...) {
    ssm_stan_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for ARIMA models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `forecast::Arima()`
#' 
#' @return A prediction
#'
#' @export
ssm_stan_predict_impl <- function(object, new_data, ...) {
    
    # PREPARE INPUTS
    model       <- object$models$model_1
    idx_train   <- object$data %>% timetk::tk_index()
    xreg_recipe <- object$extras$xreg_recipe
    seed        <- object$extras$pred_seed
    h_horizon   <- nrow(new_data)
    
    # XREG
    xreg_matrix <- modeltime::bake_xreg_recipe(xreg_recipe, new_data, format = "matrix")
    
    # PREDICTIONS
    if (!is.null(xreg_matrix)) {
        preds_forecast <- bayesforecast::forecast(model, h = h_horizon, xreg = xreg_matrix, seed = seed, ...)
    } else {
        preds_forecast <- bayesforecast::forecast(model, h = h_horizon, seed = seed,  ...)
    }
    
    # Return predictions as numeric vector
    preds <- tibble::as_tibble(preds_forecast) %>% purrr::pluck(1)
    
    return(preds)
    
}

