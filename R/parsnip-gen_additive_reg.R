#' Interface for Generalized Additive Models (GAM)
#'
#' @param mode A single character string for the type of model.
#' @param markov_chains Number of Markov chains (defaults to 4).
#' @param chain_iter Number of total iterations per chain (including warmup; defaults to 2000).
#' @param warmup_iter A positive integer specifying number of warmup (aka burnin) iterations. This also 
#' specifies the number of iterations used for stepsize adaptation, so warmup samples should not be used for inference. 
#' The number of warmup should not be larger than iter and the default is iter/2.
#' @param adapt_delta The thin of the jumps in a HMC method.
#'  
#' 
#' @return 
#' A `parsnip` model specification 
#' 
#' @details 
#' 
#' __Available Engines:__
#' - __stan__: Connects to `brms::brm()`
#' 
#' 
#' @section Engine Details:
#' 
#' __stan__
#' 
#' This engine uses [brms::brm()] and has the following parameters, 
#' which can be modified through the [parsnip::set_engine()] function. 
#' 
#' ``` {r echo=F}
#' str(brms::brm)
#' ```
#' 
#' @section Fit Details:
#' 
#' __BRMS Formula Interface__
#' 
#' Fitting GAMs is accomplished using parameters including:
#' 
#' - [brms::s()]: GAM spline smooths
#' - [brms::t2()]: GAM tensor product smooths
#' 
#' These are applied in the `fit()` function:
#' 
#' ``` r
#' fit(value ~ s(date_mon, k = 12) + s(date_num), data = df)
#' ```
#' @return A model spec
#' 
#' @examples 
#' 
#' \dontrun{
#' library(tidymodels)
#' library(bayesmodels)
#' library(modeltime)
#' library(tidyverse)
#' library(timetk)
#' library(lubridate)
#' 
#' m750_extended <- m750 %>%
#'     group_by(id) %>%
#'     future_frame(.length_out = 24, .bind_data = TRUE) %>%
#'     mutate(lag_24 = lag(value, 24)) %>%
#'     ungroup() %>%
#'     mutate(date_num = as.numeric(date)) %>%
#'     mutate(date_month = month(date))
#' 
#' m750_train  <- m750_extended %>% drop_na()
#' m750_future <- m750_extended %>% filter(is.na(value))
#' 
#' model_fit_gam <- gen_additive_reg(mode = "regression", markov_chains = 2) %>%
#'     set_engine("stan", family=Gamma(link="log")) %>%
#'     fit(value ~  date + s(date_month, k = 12)
#'         + s(lag_24),
#'         data = m750_train) 
#'  }
#' @export
gen_additive_reg <- function(mode = "regression", 
                             markov_chains = NULL,
                             chain_iter = NULL,
                             warmup_iter = NULL,
                             adapt_delta = NULL) {
    
    args <- list(
        markov_chains   = rlang::enquo(markov_chains),
        chain_iter      = rlang::enquo(chain_iter),
        warmup_iter     = rlang::enquo(warmup_iter),
        adapt_delta     = rlang::enquo(adapt_delta)
    )
    
    parsnip::new_model_spec(
        "gen_additive_reg",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )
    
}

#' @export
print.gen_additive_reg <- function(x, ...) {
    cat("GAM Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)
    
    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }
    
    invisible(x)
}

#' @export
#' @importFrom stats update
update.gen_additive_reg <- function(object,
                                    markov_chains = NULL,
                                    chain_iter = NULL,
                                    warmup_iter = NULL,
                                    adapt_delta = NULL,
                                    parameters = NULL,
                                    fresh = FALSE, ...) {
    
    parsnip::update_dot_check(...)
    
    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }
    
    args <- list(
        markov_chains   = rlang::enquo(markov_chains),
        chain_iter      = rlang::enquo(chain_iter),
        warmup_iter     = rlang::enquo(warmup_iter),
        adapt_delta     = rlang::enquo(adapt_delta)
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
        "gen_additive_reg",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.gen_additive_reg <- function(x, engine = x$engine, ...) {
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
#' @param formula A dataframe of xreg (exogenous regressors)
#' @param data A numeric vector of values to fit
#' @param chains An integer of the number of Markov Chains chains to be run, by default 4 chains are run.
#' @param iter An integer of total iterations per chain including the warm-up, by default the number of iterations are 2000.
#' @param warmup A positive integer specifying number of warm-up (aka burn-in) iterations. This also specifies the number of iterations used for step-size adaptation, so warm-up samples should not be used for inference. The number of warmup should not be larger than iter and the default is iter/2.
#' @param ... Additional arguments passed to `forecast::Arima`
#' 
#' @return A modeltime model
#'
#' @export
gen_additive_stan_fit_impl <- function(formula, data, chains = 4, iter = 2000, warmup = 1000, ...) {
    
    args <- list(...)
    
    if (!any(names(args) %in% "cores")) {
        args[["cores"]] <- 1
    }
    
    if (!any(names(args) %in% "thin")) {
        args[["thin"]] <- 10
    }
    
    if (!any(names(args) %in% "control")) {
        args[["control"]] <- list(adapt_delta = 0.99)
    }
    
    args[["data"]] <- data
    args[["chains"]] <- chains
    args[["iter"]] <- iter 
    args[["warmup"]] <- warmup
    
    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    
    y <- all.vars(formula)[1]
    x <- attr(stats::terms(formula, data = data), "term.labels")
    x1 <- all.vars(formula)[2:length(all.vars(formula))]
    # 
    outcome <- data[[y]]
    predictors <- data %>% dplyr::select(dplyr::all_of(x1))
    # 
    # # INDEX & PERIOD
    # # Determine Period, Index Col, and Index
    index_tbl <- modeltime::parse_index_from_data(predictors)
    period    <- modeltime::parse_period_from_index(index_tbl, "auto")
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)
    
    var <- if(length(x %>% dplyr::setdiff(idx_col)) == 0) {x} else {x %>% dplyr::setdiff(idx_col)}
    
    formula <- stats::reformulate(termlabels = var,
                                  response   = y)
    
    args[["formula"]] <- brms::bf(formula)
    
    model_call <-parsnip::make_call(fun  = "brm",
                                    ns   = "brms",
                                    args = args)
    
    model_gam <- rlang::eval_tidy(model_call, env = rlang::current_env())
    
    # RETURN
    modeltime::new_modeltime_bridge(
        class = "gen_additive_stan_fit_impl",
        
        # Models
        models = list(
            model_1 = model_gam
        ),
        
        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            idx_col      := idx,
            .actual      =  outcome,
            .fitted      =  .actual - residuals(model_gam) %>% tibble::as_tibble() %>% dplyr::select(Estimate) %>% .$Estimate,
            .residuals   =  residuals(model_gam) %>% tibble::as_tibble() %>% dplyr::select(Estimate) %>% .$Estimate
        ),
        
        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(
            args = args
        ),
        
        # Description - Convert arima model parameters to short description
        desc = "Bayesian Generalized Additive Model"
    )
    
}

#' @export
print.gen_additive_stan_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}



#' @export
predict.gen_additive_stan_fit_impl <- function(object, new_data, ...) {
    gen_additive_stan_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for ARIMA models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `forecast::Arima()`
#' 
#' @return A prediction
#'
#' @export
gen_additive_stan_predict_impl <- function(object, new_data, ...) {
    
    # PREPARE INPUTS
    model       <- object$models$model_1
    
    new_data    <- new_data %>% dplyr::mutate_if(is_date_class, as.numeric)
    
    preds_forecast <- stats::predict(model, newdata = new_data, ...)
    
    # Return predictions as numeric vector
    preds <- tibble::as_tibble(preds_forecast) %>% dplyr::pull(1) %>% as.numeric()
    
    return(preds)
    
}

