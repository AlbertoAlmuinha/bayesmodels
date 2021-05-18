#' General Interface for Naive and Random Walk models Regression Models
#'
#' `random_walk_reg()` is a way to generate a _specification_ of Naive and Random Walk models 
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `bayesforecast`.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param seasonal_random_walk a Boolean value for select a seasonal random walk instead.
#' @param seasonal_period an optional integer value for the seasonal period.
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
#'  to determine the _mode_ of the model. For `random_walk_reg()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - "stan" (default) - Connects to [bayesforecast::stan_naive()]
#'
#' __Main Arguments__
#'
#' The main arguments (tuning parameters) for the model are:
#'
#'  - `seasonal_random_walk`: a Boolean value for select a seasonal random walk instead.
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
#'    ~"bayesmodels", ~"bayesforecast::stan_naive",
#'    "seasonal_random_walk", "seasonal",
#'    "markov_chains", "chains(4)",
#'    "adapt_delta", "adapt.delta(0.9)",
#'    "tree_depth", "tree.depth(10)"
#' ) %>% knitr::kable()
#' ```
#'
#' Other options can be set using `set_engine()`.
#'
#' __stam (default engine)__
#'
#' The engine uses [bayesforecast::stan_naive()].
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
#' model_spec <- random_walk_reg() %>%
#'     set_engine("stan")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#' model_fit
#' }
#' @export
random_walk_reg <- function(mode = "regression", seasonal_random_walk = NULL, seasonal_period = NULL, markov_chains = NULL,
                    chain_iter = NULL, warmup_iter = NULL, adapt_delta = NULL, tree_depth = NULL, pred_seed = NULL) {
    
    args <- list(
        seasonal_random_walk      = rlang::enquo(seasonal_random_walk),
        seasonal_period           = rlang::enquo(seasonal_period),
        
        markov_chains             = rlang::enquo(markov_chains),
        chain_iter                = rlang::enquo(chain_iter),
        warmup_iter               = rlang::enquo(warmup_iter),
        adapt_delta               = rlang::enquo(adapt_delta),
        tree_depth                = rlang::enquo(tree_depth),
        
        pred_seed                 = rlang::enquo(pred_seed)
    )
    
    parsnip::new_model_spec(
        "random_walk_reg",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )
    
}

#' @export
print.random_walk_reg <- function(x, ...) {
    cat("Naive Random Walk Regression Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)
    
    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }
    
    invisible(x)
}

#' @export
#' @importFrom stats update
update.random_walk_reg <- function(object, parameters = NULL,
                           seasonal_random_walk = NULL, seasonal_period = NULL, markov_chains = NULL, 
                           chain_iter = NULL, warmup_iter = NULL, adapt_delta = NULL, tree_depth = NULL, pred_seed = NULL,
                           fresh = FALSE, ...) {
    
    parsnip::update_dot_check(...)
    
    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }
    
    args <- list(
        seasonal_random_walk      = rlang::enquo(seasonal_random_walk),
        seasonal_period           = rlang::enquo(seasonal_period),
        
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
        "random_walk_reg",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.random_walk_reg <- function(x, engine = x$engine, ...) {
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
#' @param seasonal a Boolean value for select a seasonal random walk instead
#' @param m an optional integer value for the seasonal period.
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
random_walk_stan_fit_impl <- function(x, y, seasonal = FALSE, m = 0,
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

    
    # FIT
    outcome <- stats::ts(outcome, frequency = period)
    
    fit_naive   <- bayesforecast::stan_naive(outcome, 
                                             seasonal      = seasonal,
                                             m             = m,  
                                             chains        = chains,
                                             iter          = iter,
                                             warmup        = warmup,
                                             tree.depth    = tree.depth,
                                             adapt.delta   = adapt.delta,
                                             ...)

    
    # RETURN
    modeltime::new_modeltime_bridge(
        class = "random_walk_stan_fit_impl",
        
        # Models
        models = list(
            model_1 = fit_naive
        ),
        
        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            !! idx_col  := idx %>% rev() %>% .[1:length(as.numeric(residuals(fit_naive)))],
            .actual      =  as.numeric(fit_naive$model$yreal) %>% rev() %>% .[1:length(as.numeric(residuals(fit_naive)))] %>% rev(),
            .fitted      =  .actual - as.numeric(residuals(fit_naive)),
            .residuals   =  as.numeric(residuals(fit_naive))
        ),
        
        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(
            pred_seed   = seed
        ),
        
        # Description - Convert arima model parameters to short description
        desc = "Naive Model"
    )
    
}

#' @export
print.random_walk_stan_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}



#' @export
predict.random_walk_stan_fit_impl <- function(object, new_data, ...) {
    random_walk_stan_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for ARIMA models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `forecast::Arima()`
#' 
#' @return A prediction
#'
#' @export
random_walk_stan_predict_impl <- function(object, new_data, ...) {
    
    # PREPARE INPUTS
    model       <- object$models$model_1
    idx_train   <- object$data %>% timetk::tk_index()
    seed        <- object$extras$pred_seed
    h_horizon   <- nrow(new_data)
    
    
    preds_forecast <- bayesforecast::forecast(model, h = h_horizon, seed = seed, ...)
    
    # Return predictions as numeric vector
    preds <- tibble::as_tibble(preds_forecast) %>% purrr::pluck(1)
    
    return(preds)
    
}

