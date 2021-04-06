# garch_reg() - General Interface GARCH Models
#' General Interface for GARCH Models
#'
#' @param mode A single character string for the type of model.
#' @param arch_order An integer giving the order of the ARCH part for the variance model. Applies to both garch and rugarch engines.
#' @param garch_order An integer giving the order of the GARCH part for the variance model. Applies to both garch and rugarch engines.
#' @param ar_order An integer giving the order of the AR part for the mean model. Only applies to rugarch engine.
#' @param ma_order An integer giving the order of the MA part for the mean model. Only applies to rugarch engine.
#' 
#' These arguments are converted to their specific names at the
#'  time that the model is fit.
#'
#' Other options and argument can be
#'  set using `set_engine()` (See Engine Details below).
#' 
#' @details 
#' 
#' Available engines:
#' - __rugarch__: Connects to `rugarch::ugarchspec()` first and then to `rugarch::ugarchfit()`.
#' 
#' @section Engine Details:
#' 
#' __rugarch (default)__
#' 
#' The engine uses [rugarch::ugarchspec()] and [rugarch::ugarchfit()].
#' 
#' Function Parameters:
#'  ```{r echo = FALSE}
#' str(rugarch::ugarchspec)
#' ```
#' 
#' The Garch order for the variance model is provided using `arch_order` and `garch_order` parameters..
#' The ARMA order for the mean model is provided using `ar_order` and `ma_order` parameters.
#' Other options and arguments can be set using `set_engine()`.
#' 
#' #' Parameter Notes:
#' - `xreg` - This engine supports xregs for both the variance model and the mean model. You can do this in two ways, 
#' either enter the matrices through set_engine parameters or as a formula in fit (note that the latter option is more limited, 
#' since you will not be able to pass two different xregs, one for each model). For simpler cases this is a compact option.
#' - `order parameters` - The parameters of rugarch::ugarchspec are lists containing several elements, 
#' some of them the commands that are the main arguments of the function. If you want to modify the parameter 
#' that encompasses such a list, you must know that the parameter passed in the function parameter will always prevail. 
#' (See Examples).
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
#'  The `month.lbl` is an exogenous regressor that can be passed to the `garch_reg()` using
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
#' 
#' @examples 
#' \dontrun{
#' library(tidymodels)
#' library(garchmodels)
#' library(modeltime)
#' library(tidyverse)
#' library(timetk)
#' library(lubridate)
#' 
#' rIBM_extended <- rIBM %>%
#'     future_frame(.length_out = 24, .bind_data = TRUE) 
#' 
#' rIBM_train  <- rIBM_extended %>% drop_na()
#' rIBM_future <- rIBM_extended %>% filter(is.na(daily_returns))
#' 
#' model_garch_fit <-garchmodels::garch_reg(mode = "regression",
#'                                           arch_order = 1,
#'                                           garch_order = 1) %>%
#'     set_engine("rugarch") %>%
#'     fit(daily_returns ~ date, data = rIBM_train)
#' 
#' predict(model_garch_fit, rIBM_future)
#' 
#' model_garch_fit <-garchmodels::garch_reg(mode = "regression",
#'                                         arch_order = 2,
#'                                         garch_order = 2) %>%
#'     set_engine("rugarch", variance.model = list(model='gjrGARCH', 
#'                                                 garchOrder=c(1,1)),
#'                mean.model     = list(armaOrder=c(0,0))) %>%
#'     fit(daily_returns ~ date, data = rIBM_train)
#' 
#' predict(model_garch_fit, rIBM_future)
#' } 
#' @export
garch_reg <- function(mode = "regression",
                      arch_order = NULL,
                      garch_order = NULL,
                      ar_order = NULL,
                      ma_order = NULL) {
    
    args <- list(
        arch_order  = rlang::enquo(arch_order),
        garch_order = rlang::enquo(garch_order),
        ar_order    = rlang::enquo(ar_order),
        ma_order    = rlang::enquo(ma_order)
    )
    
    parsnip::new_model_spec(
        "garch_reg",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )
    
}

#' @export
print.garch_reg <- function(x, ...) {
    cat("GARCH Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)
    
    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }
    
    invisible(x)
}

#' @export
#' @importFrom stats update
update.garch_reg <- function(object,
                             arch_order = NULL,
                             garch_order = NULL,
                             ar_order = NULL,
                             ma_order = NULL,
                             parameters = NULL,
                             fresh = FALSE, ...) {
    
    parsnip::update_dot_check(...)
    
    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }
    
    args <- list(
        arch_order = rlang::enquo(arch_order),
        garch_order = rlang::enquo(garch_order),
        ar_order = rlang::enquo(ar_order),
        ma_order = rlang::enquo(ma_order)
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
        "garch_reg",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.garch_reg <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'rugarch'` for translation.")
        engine <- "rugarch"
    }
    x <- parsnip::translate.default(x, engine, ...)
    
    x
}

#' # FIT - GARCH -----
#' 
#' #' Low-Level GARCH function for translating modeltime to forecast
#' #'
#' #' @param formula A dataframe of xreg (exogenous regressors)
#' #' @param data A numeric vector of values to fit
#' #' @param a The order of the non-seasonal auto-regressive (AR) terms. Often denoted "p" in pdq-notation.
#' #' @param g The order of the non-seasonal auto-regressive (AR) terms. Often denoted "p" in pdq-notation.
#' #' @param ... Additional arguments passed to `forecast::Arima`
#' #'
#' #' @export
#' garch_fit_impl <- function(formula, data, a = 1, g = 1, ar_no_apply = NULL, ma_no_apply = NULL, period = "auto", ...) {
#'     
#'     # X & Y
#'     others <- list(...)
#'     
#'      y <- all.vars(formula)[1]
#'      x <- attr(stats::terms(formula, data = data), "term.labels")
#'     
#'     outcome <- data[[y]]
#'     predictors <- data %>% dplyr::select(dplyr::all_of(x))
#'     
#'     # INDEX & PERIOD
#'     # Determine Period, Index Col, and Index
#'     index_tbl <- modeltime::parse_index_from_data(predictors)
#'     period    <- modeltime::parse_period_from_index(index_tbl, period)
#'     idx_col   <- names(index_tbl)
#'     idx       <- timetk::tk_index(index_tbl)
#'     
#'     # XREGS
#'     # Clean names, get xreg recipe, process predictors
#'     # xreg_recipe <- create_xreg_recipe(predictor, prepare = TRUE)
#'     # xreg_matrix <- juice_xreg_recipe(xreg_recipe, format = "matrix")
#'     
#'     # FIT
#'     outcome <- stats::ts(outcome, frequency = period)
#'     
#'     fit_garch <- tseries::garch(outcome, order = c(a, g), ...)
#'     
#'     # RETURN
#'     modeltime::new_modeltime_bridge(
#'         class = "garch_fit_impl",
#'         
#'         # Models
#'         models = list(
#'             model_1 = fit_garch
#'         ),
#'         
#'         # Data - Date column (matches original), .actual, .fitted, and .residuals columns
#'         data = tibble::tibble(
#'             !! idx_col  := idx,
#'             .actual      =  as.numeric(outcome),
#'             .fitted      =  fit_garch$fitted.values[,1],
#'             .residuals   =  fit_garch$residuals
#'         ),
#'         
#'         extras = list(
#'             y_var    = y,
#'             period   = period,
#'             otros    = others
#'         ),
#'         
#'         # Description - Convert arima model parameters to short description
#'         desc = stringr::str_glue('GARCH ({fit_garch$order[1]}, {fit_garch$order[2]}) Model')
#'     )
#'     
#' }
#' 
#' #' @export
#' print.garch_fit_impl <- function(x, ...) {
#'     print(x$models$model_1)
#'     invisible(x)
#' }


# FIT - GARCH -----

#' Low-Level GARCH function for translating modeltime to forecast
#'
#' @param formula A dataframe of xreg (exogenous regressors)
#' @param data A numeric vector of values to fit
#' @param a The order of ARCH part
#' @param g The order of GARCH part
#' @param ar The order of the non-seasonal auto-regressive (AR) terms. Often denoted "p" in pdq-notation.
#' @param ma The order of the non-seasonal auto-regressive (AR) terms. Often denoted "p" in pdq-notation.
#' @param period Period
#' @param ... Additional arguments passed to `forecast::Arima`
#'
#' @export
rugarch_fit_impl <- function(formula, data, a = 1, g = 1, ar = 1, ma = 1, period = "auto", ...) {
    
    # X & Y
    others <- list(...)
    
    if (any(names(others) %in% "variance.model")) {
        
        others$variance.model$garchOrder <- c(a, g)
        
    } else { #Defaults Settings
        
        others[['variance.model']] <- list(model = "sGARCH", 
                                           garchOrder = c(a, g), 
                                           submodel = NULL, 
                                           external.regressors = NULL, 
                                           variance.targeting = FALSE)
        
    }
    
    
    if (any(names(others) %in% "mean.model")) {
        
        others$mean.model$armaOrder <- c(ar, ma)
        
    } else { #Defaults Settings
        
        others[['mean.model']] <- list(armaOrder = c(ar, ma), include.mean = TRUE, archm = FALSE, 
                                       archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE)
        
    }
    
    y <- all.vars(formula)[1]
    x <- attr(stats::terms(formula, data = data), "term.labels")
    
    outcome <- data[[y]]
    predictors <- data %>% dplyr::select(dplyr::all_of(x))
    
    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- modeltime::parse_index_from_data(predictors)
    period    <- modeltime::parse_period_from_index(index_tbl, period)
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)
    
    # XREGS
    # Clean names, get xreg recipe, process predictors
    xreg_recipe <- modeltime::create_xreg_recipe(predictors, prepare = TRUE)
    xreg_matrix <- modeltime::juice_xreg_recipe(xreg_recipe, format = "matrix")
    
    safe_is_null <- purrr::safely(function(x){is.null(x)}, otherwise = NA, quiet = TRUE)
    
    if (!is.null(xreg_matrix)){
        
        if (any(names(others) %in% "variance.model")){
            
            if (safe_is_null(others$variance.model$external.regressors) %>% .$result == F){
                others$variance.model$external.regressors <- as.matrix(xreg_matrix)
            }
            
        }
        
        if (any(names(others) %in% "mean.model")){
            
            if (safe_is_null(others$mean.model$external.regressors) %>% .$result == F){
                others$mean.model$external.regressors <- as.matrix(xreg_matrix)
            }
            
        }
        
    }
    
    #UGSPEC
    
    ugspec <-parsnip::make_call(fun  = "ugarchspec",
                                ns   = "rugarch",
                                args = others)
    
    ugspec <-rlang::eval_tidy(ugspec)
    
    # FIT
    #outcome <- stats::ts(outcome, frequency = period)
    outcome <- data.frame(outcome = outcome)
    rownames(outcome)<-idx
    
    fit_garch <- rugarch::ugarchfit(ugspec, data = outcome, ...)
    
    # RETURN
    modeltime::new_modeltime_bridge(
        class = "rugarch_fit_impl",
        
        # Models
        models = list(
            model_1 = fit_garch
        ),
        
        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            !! idx_col  := idx,
            .actual      =  as.numeric(outcome$outcome),
            .fitted      =  fit_garch@fit$fitted.values,
            .residuals   =  fit_garch@fit$residuals
        ),
        
        extras = list(
            y_var    = y,
            period   = period
        ),
        
        # Description - Convert garch model parameters to short description
        desc = stringr::str_glue('Variance {fit_garch@model$modeldesc$vmodel} ({a}, {g}) - ({stringr::str_to_title(fit_garch@model$modeldesc$distribution)}) Model')
    )
    
}

#' @export
print.rugarch_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}

# PREDICT ----

#' @export
predict.rugarch_fit_impl <- function(object, new_data, ...) {
    rugarch_predict_impl(object, new_data, ...)
}


#' Bridge prediction function for GARCH models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `stats::predict()`
#'
#' @export
rugarch_predict_impl <- function(object, new_data, ...) {
    
    # PREPARE INPUTS
    model       <- object$models$model_1
    # y_var       <- object$extras$y_var
    # index       <- object$extras$y 
    # period      <- object$extras$period
    # 
    # outcome     <- new_data %>% dplyr::select({{ y_var }})
    # 
    # outcome <- stats::ts(outcome, frequency = period)
    
    # PREDICTIONS
    
    preds_forecast <- rugarch::ugarchforecast(model, n.ahead = nrow(new_data), ...) 
    
    preds_forecast <- tibble::tibble(cast@forecast) %>% 
                      tibble::rowid_to_column("rowid") %>% 
                      dplyr::filter(rowid == 5 | rowid == 6) %>%
                      purrr::set_names(c("rowid", ".pred")) %>%
                      dplyr::mutate(.name = c("sigmaFor", "seriesFor")) %>%
                      dplyr::relocate(".name", .before = .pred) %>%
                      dplyr::select(.name, .pred)
    
    return(preds_forecast)
    
}































