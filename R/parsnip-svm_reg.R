#' General Interface for Stochastic Volatility Regression Models
#'
#' `svm_reg()` is a way to generate a _specification_ of a Stochastic volatility model 
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `bayesforecast`.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param non_seasonal_ar The order of the non-seasonal auto-regressive (AR) terms. Often denoted "p" in pdq-notation.
#' @param non_seasonal_ma The order of the non-seasonal moving average (MA) terms. Often denoted "q" in pdq-notation
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
#'  to determine the _mode_ of the model. For `svm_reg()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - "stan" (default) - Connects to [bayesforecast::stan_SVM()]
#'
#' __Main Arguments__
#'
#' The main arguments (tuning parameters) for the model are:
#'
#'  - `non_seasonal_ar`: The order of the non-seasonal auto-regressive (AR) terms.
#'  - `non_seasonal_ma`: The order of the non-seasonal moving average (MA) terms.
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
#'    ~"bayesmodels", ~"bayesforecast::stan_SVM",
#'    "non_seasonal_ar, non_seasonal_ma", "arma(0, 0)", 
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
#' The engine uses [bayesforecast::stan_SVM()].
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
#'
#' # Model Spec
#' model_spec <- svm_reg() %>%
#'     set_engine("stan")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#' model_fit
#'
#' }
#' @export
svm_reg <- function(mode = "regression", non_seasonal_ar = NULL, non_seasonal_ma = NULL, markov_chains = NULL,
                      chain_iter = NULL, warmup_iter = NULL, adapt_delta = NULL, tree_depth = NULL, pred_seed = NULL) {
    
    args <- list(
        non_seasonal_ar           = rlang::enquo(non_seasonal_ar),
        non_seasonal_ma           = rlang::enquo(non_seasonal_ma),
        
        markov_chains             = rlang::enquo(markov_chains),
        chain_iter                = rlang::enquo(chain_iter),
        warmup_iter               = rlang::enquo(warmup_iter),
        adapt_delta               = rlang::enquo(adapt_delta),
        tree_depth                = rlang::enquo(tree_depth),
        
        pred_seed                 = rlang::enquo(pred_seed)
    )
    
    parsnip::new_model_spec(
        "svm_reg",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )
    
}

#' @export
print.svm_reg <- function(x, ...) {
    cat("SVM Regression Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)
    
    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }
    
    invisible(x)
}

#' @export
#' @importFrom stats update
update.svm_reg <- function(object, parameters = NULL,
                           non_seasonal_ar = NULL, non_seasonal_ma = NULL, markov_chains = NULL,
                           chain_iter = NULL, warmup_iter = NULL, adapt_delta = NULL, tree_depth = NULL, pred_seed = NULL,
                           fresh = FALSE, ...) {
    
    parsnip::update_dot_check(...)
    
    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }
    
    args <- list(
        non_seasonal_ar           = rlang::enquo(non_seasonal_ar),
        non_seasonal_ma           = rlang::enquo(non_seasonal_ma),
        
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
        "svm_reg",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.svm_reg <- function(x, engine = x$engine, ...) {
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
#' @param p The order of the non-seasonal auto-regressive (AR) terms. Often denoted "p" in pdq-notation.
#' @param q The order of the non-seasonal moving average (MA) terms. Often denoted "q" in pdq-notation.
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
svm_stan_fit_impl <- function(x, y, p = 0, q = 0, chains = 4, iter = 2000, warmup = iter/2, 
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
        fit_svm   <- bayesforecast::stan_SVM(outcome, 
                                             arma        = c(p, q),
                                             xreg        = xreg_matrix,  
                                             chains      = chains,
                                             iter        = iter,
                                             warmup      = warmup,
                                             tree.depth  = tree.depth,
                                             adapt.delta = adapt.delta,
                                             ...)
    } else {
        fit_svm   <- bayesforecast::stan_SVM(outcome, 
                                             arma        = c(p, q),
                                             chains      = chains,
                                             iter        = iter,
                                             warmup      = warmup,
                                             tree.depth  = tree.depth,
                                             adapt.delta = adapt.delta,
                                             ...)
    }
    
    # RETURN
    modeltime::new_modeltime_bridge(
        class = "svm_stan_fit_impl",
        
        # Models
        models = list(
            model_1 = fit_svm
        ),
        
        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            !! idx_col  := idx,
            .actual      =  as.numeric(fit_svm$model$yreal),
            .fitted      =  .actual - as.numeric(residuals(fit_svm)),
            .residuals   =  as.numeric(residuals(fit_svm))
        ),
        
        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(
            xreg_recipe = xreg_recipe,
            pred_seed   = seed
        ),
        
        # Description - Convert arima model parameters to short description
        desc = "Stochastic volatility Model"
    )
    
}

#' @export
print.svm_stan_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}



#' @export
predict.svm_stan_fit_impl <- function(object, new_data, ...) {
    svm_stan_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for ARIMA models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `forecast::Arima()`
#' 
#' @return A prediction
#'
#' @export
svm_stan_predict_impl <- function(object, new_data, ...) {
    
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

