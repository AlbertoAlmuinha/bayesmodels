# garch_multivariate_reg() - General Interface for Multivariate GARCH Models
#' General Interface for Multivariate GARCH Models
#' 
#' `garch_multivariate_reg()` allows you to model the volatility of various time series. This can be done with the multivariate equivalent 
#' of the univariate GARCH model. Estimating multivariate GARCH models turns out to be significantly more difficult than univariate GARCH models, 
#' but this function facilitates the task through different engines such as rugarch, dcc_rmgarch, gogar_rmgarch etc.
#'
#' @param mode A single character string for the type of model (Only regression is supported).
#' @param type A single character string for the type of model or specification (See details below).
#'
#' Other options and argument can be set using `set_engine()` (See Engine Details below).
#' 
#' 
#' @details 
#' 
#' Available engines:
#' - __rugarch (default)__: Connects to `rugarch::multispec()` and `rugarch::multifit()`
#' - __dcc_rmgarch__: Connects to `rugarch::multispec()`, `rugarch::multifit()`, `rmgarch::dccspec()` and `rmgarch::dccfit()`.
#' - __c_rmgarch__: Connects to `rugarch::multispec()`, `rugarch::multifit()`, `rmgarch::cgarchspec()` and `rmgarch::cgarchfit()`.
#' - __gogarch_rmgarch__: Connects to `rmgarch::gogarchspec()` and `rmgarch::gogarchspec()`.
#' 
#' 
#' 
#' @section Engine Details:
#' 
#' __rugarch (default)__
#' 
#' The engine uses [rugarch::multispec()] and then [rugarch::multifit()]
#' 
#' __Main Arguments__
#' 
#' - `type`: You can choose between `ugarchspec` (default) or `arfimaspec`. Depending on which one you choose, 
#' you will select either a univariate GARCH model for each of your variables or an Arfima model as specification, 
#' which will then be passed to `rugarch::multispec()`.
#'
#' You __must__ pass an argument through `set_engine()` called __specs__ which will 
#' be a list consisting of the arguments to be passed to each of the specifications used 
#' in `rugarch::multispec()`. Other arguments that you wish to pass to `rugarch::multifit()` can 
#' also be passed through `set_engine()`
#' 
#' For example, imagine you have a data frame with 3 variables. For each of those variables you must 
#' define a specification (you can check the arguments you can use for a specification in ?rugarch::ugarchspec). 
#' Once the specifications have been decided, the way to pass it through set_engine would be as follows:
#' 
#' garch_multivariate_reg(mode = "regression") %>%
#'     set_engine("rugarch" , specs = list(spec1 = list(mean.model = list(armaOrder = c(1,0))), 
#'                                         spec2 = list(mean.model = list(armaOrder = c(1,0))),
#'                                         spec3 = list(mean.model = list(armaOrder = c(1,0)))),
#'                out.sample = 10)
#' 
#' In the fit section we will see how to pass variables through parsnip::fit (See Fit Section below).
#'
#' Parameter Notes:
#' - `xreg` - This engine does support xregs, but you have to provide them to each model in an 
#' array through set_engine. For more information see ?rugarch::ugarchspec. The xregs can be provided
#' through `variance.model$external.regressors` or `mean.model$external.regressors` (or both) for the 
#' specifications of the desired variables.
#' 
#' 
#' __dcc_rmgarch__
#' 
#' The engine uses [rugarch::multispec()], [rugarch::multifit()], [rmgarch::dccspec()] and [rmgarch::dccfit()].
#' 
#' __Main Arguments__
#' 
#' - `type`: Only `ugarchspec` is supported for this engine. This will then be passed to `rugarch::multispec()`.
#'
#' You __must__ pass an argument through `set_engine()` called __specs__ which will 
#' be a list consisting of the arguments to be passed to each of the specifications used 
#' in `rugarch::multispec()`. Other arguments that you wish to pass to `rugarch::multifit()` can 
#' also be passed through `set_engine()`. To pass arguments to `dccfit()` you __must__ pass a list through 
#' `set_engine` called __dcc_specs__.
#' 
#' For example, imagine you have a data frame with 3 variables. For each of those variables you must 
#' define a specification (you can check the arguments you can use for a specification in ?rugarch::ugarchspec). 
#' Once the specifications have been decided, the way to pass it through set_engine would be as follows:
#' 
#' garch_fit_model <- garch_multivariate_reg(type = "ugarchspec") %>%
#'     set_engine("dcc_rmgarch" , specs = list(spec1 = list(mean.model = list(armaOrder = c(1,0))), 
#'                                             spec2 = list(mean.model = list(armaOrder = c(1,0))),
#'                                             spec3 = list(mean.model = list(armaOrder = c(1,0)))),
#'                                dcc_specs = list(dccOrder = c(2,2), distribution = "mvlaplace"))
#' 
#' In the fit section we will see how to pass variables through parsnip::fit (See Fit Section below).
#' 
#' 
#' __c_rmgarch__
#' 
#' The engine uses [rugarch::multispec()], [rugarch::multifit()], [rmgarch::cgarchspec()] and [rmgarch::cgarchfit()].
#' 
#' __Main Arguments__
#' 
#' - `type`: Only `ugarchspec` is supported for this engine. This will then be passed to `rugarch::multispec()`.
#'
#' You __must__ pass an argument through `set_engine()` called __specs__ which will 
#' be a list consisting of the arguments to be passed to each of the specifications used 
#' in `rugarch::multispec()`. Other arguments that you wish to pass to `rugarch::multifit()` can 
#' also be passed through `set_engine()`. To pass arguments to `cgarchfit()` you __must__ pass a list through 
#' `set_engine` called __c_specs__.
#' 
#' For example, imagine you have a data frame with 3 variables. For each of those variables you must 
#' define a specification (you can check the arguments you can use for a specification in ?rugarch::ugarchspec). 
#' Once the specifications have been decided, the way to pass it through set_engine would be as follows:
#' 
#' garch_fit_model <- garch_multivariate_reg(type = "arfima") %>%
#'     set_engine("c_rmgarch" , specs = list(spec1 = list(mean.model = list(armaOrder = c(1,0))), 
#'                                           spec2 = list(mean.model = list(armaOrder = c(1,0))),
#'                                           spec3 = list(mean.model = list(armaOrder = c(1,0)))),
#'                              c_specs = list(dccOrder = c(2,2))) %>%
#'     fit(value ~ date + id, data = rX_longer_train)
#' 
#' In the fit section we will see how to pass variables through parsnip::fit (See Fit Section below).
#' 
#' 
#' __gogarch_rmgarch__
#' 
#' The engine uses [rmgarch::gogarchspec()] and [rmgarch::gogarchfit()].
#' 
#' __Main Arguments__
#' 
#' - `type`: Not available for this engine.
#'
#' You __must__ pass an argument through `set_engine()` called __gogarch_specs__ which will 
#' be a list consisting of the arguments to be passed to each of the specifications used 
#' in `rmgarch::gogarchspec()`. Other arguments that you wish to pass to `rmgarch::gogarchfit()` can 
#' also be passed through `set_engine()`. 
#' 
#' For example, imagine you have a data frame with 3 variables. For each of those variables you must 
#' define a specification (you can check the arguments you can use for a specification in ?rugarch::ugarchspec). 
#' Once the specifications have been decided, the way to pass it through set_engine would be as follows:
#' 
#' model_fit_garch <- garch_multivariate_reg(type = "ugarchspec") %>%
#'     set_engine("gogarch_rmgarch" , gogarch_specs = list(variance.model = list(garchOrder = c(2,2)))) %>%
#'     fit(value ~ date + id, data = rX_longer_train)
#' 
#' In the fit section we will see how to pass variables through parsnip::fit (See Fit Section below).
#'
#'
#' @seealso [fit.model_spec()], [set_engine()]
#' 
#' 
#' @examples 
#' \donttest{
#' library(tidymodels)
#' library(garchmodels)
#' library(modeltime)
#' library(tidyverse)
#' library(timetk)
#' library(lubridate)
#' 
#' rX_longer <-  rX_longer %>%
#'     dplyr::mutate(date = as.Date(date)) %>%
#'     group_by(id) %>%
#'     future_frame(.length_out = 3, .bind_data = TRUE) %>%
#'     ungroup()
#' 
#' rX_longer_train  <- rX_longer %>% drop_na()
#' rX_longer_future <- rX_longer %>% filter(is.na(value))
#' 
#' #RUGARCH ENGINE
#' 
#' model_fit_garch <- garch_multivariate_reg() %>%
#'     set_engine("rugarch" , specs = list(spec1 = list(mean.model = list(armaOrder = c(1,0))), 
#'                                         spec2 = list(mean.model = list(armaOrder = c(1,0))),
#'                                         spec3 = list(mean.model = list(armaOrder = c(1,0))))) %>%
#'     fit(value ~ date + id, data = rX_longer_train)
#' 
#' predict(model_fit_garch, new_data = rX_longer_future)
#' 
#' 
#' #DCC ENGINE
#'
#'  model_fit_garch <- garch_multivariate_reg(type = "ugarchspec") %>%
#' set_engine("dcc_rmgarch" , specs = list(spec1 = list(mean.model = list(armaOrder = c(1,0))), 
#'                                         spec2 = list(mean.model = list(armaOrder = c(1,0))),
#'                                         spec3 = list(mean.model = list(armaOrder = c(1,0)))),
#'            dcc_specs = list(dccOrder = c(2,2), distribution = "mvlaplace")) %>%
#'     fit(value ~ date + id, data = rX_longer_train)
#' 
#' predict(model_fit_garch, rX_longer_future) 
#' 
#' 
#' # COPULA ENGINE
#' 
#' model_fit_garch <- garch_multivariate_reg(type = "ugarchspec") %>%
#' set_engine("c_rmgarch" , specs = list(spec1 = list(mean.model = list(armaOrder = c(1,0))),
#'                                       spec2 = list(mean.model = list(armaOrder = c(1,0))),
#'                                       spec3 = list(mean.model = list(armaOrder = c(1,0)))),
#'            c_specs = list(dccOrder = c(2,2))) %>%
#'     fit(value ~ date + id, data = rX_longer_train)
#' 
#' 
#' # GO-GARCH ENGINE
#' 
#' model_fit_garch <- garch_multivariate_reg(type = "ugarchspec") %>%
#' set_engine("gogarch_rmgarch" , gogarch_specs = list(variance.model = list(garchOrder = c(2,2)))) %>%
#'     fit(value ~ date + id, data = rX_longer_train)
#'     
#' predict(model_fit_garch, rX_longer_future)
#' }



#' @export
#' @return A model specfication
garch_multivariate_reg <- function(mode = "regression",
                                   type = NULL) {
    
    args <- list(
        type = rlang::enquo(type)
    )
    
    parsnip::new_model_spec(
        "garch_multivariate_reg",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )
    
}

#' @export
print.garch_multivariate_reg <- function(x, ...) {
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
update.garch_multivariate_reg <- function(object,
                                          type = NULL,
                                          parameters = NULL,
                                          fresh = FALSE, ...) {
    
    parsnip::update_dot_check(...)
    
    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }
    
    args <- list(
        type = rlang::enquo(type)
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
        "garch_multivariate_reg",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.garch_multivariate_reg <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'rugarch'` for translation.")
        engine <- "rugarch"
    }
    x <- parsnip::translate.default(x, engine, ...)
    
    x
}

# FIT - GARCH -----

#' Low-Level GARCH function for translating modeltime to forecast
#'
#' @param formula A dataframe of xreg (exogenous regressors)
#' @param data A numeric vector of values to fit
#' @param spec_type Must be ugarchspec or arfimaspec
#' @param period Auto
#' @param ... Additional arguments passed to `forecast::Arima`
#'
#' @export
#' @return A fitted model
rugarch_multi_fit_impl <- function(formula, data, spec_type = "ugarchspec",  period = "auto", ...) {
    
    # X & Y
    others <- list(...)
    
    spec_type <- match.arg(spec_type, choices = c('ugarchspec', 'arfimaspec'))
    
    if (!any(names(others) == "specs")){
        rlang::abort("In a multivariate garch regression, you have to specify a list 'specs' through set_engine")
    }
    
    specs <- others$specs
    
    others$specs <- NULL
    
    if (length(names(specs)) != dim(data)[2]){
        rlang::abort("The number of specifications passed through set_engine do not match the number of variables found in data.")
    }
    
    data_wider <- tidyr::pivot_wider(data, names_from = all.vars(formula)[3], values_from = all.vars(formula)[1])
    
    others[['data']] <- data_wider %>% 
                        dplyr::select(-all.vars(formula)[2]) %>%
                        as.data.frame()
    
    make_spec_call <- purrr::partial(parsnip::make_call, fun = spec_type, ns = "rugarch")
    
    specs <- purrr::map(specs, make_spec_call) %>% purrr::map(rlang::eval_tidy)
    
    others[['multispec']] <- rugarch::multispec(specs)
    
    fit_multigarch <- parsnip::make_call("multifit", "rugarch", others) %>% rlang::eval_tidy()
    
    # RETURN
    new_modelgarch_bridge(
        class = "rugarch_multi_fit_impl",
        
        # Models
        models = list(
            model_1 = fit_multigarch
        ),
        
        extras = list(variables   = names(others[['data']]),
                      index       = all.vars(formula)[2],
                      names_from  = all.vars(formula)[3],
                      values_from = all.vars(formula)[1]),
        
        # Description - Convert arima model parameters to short description
        desc = 'GARCH Multivariate Model'
    )
    
}

#' @export
print.rugarch_multi_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}


# FIT - GARCH -----

#' Low-Level GARCH function for translating modeltime to forecast
#'
#' @param formula A dataframe of xreg (exogenous regressors)
#' @param data A numeric vector of values to fit
#' @param spec_type Must be ugarchspec
#' @param period Auto
#' @param ... Additional arguments passed to `forecast::Arima`
#'
#' @export
#' @return A fitted model
dcc_rmgarch_multi_fit_impl <- function(formula, data, spec_type = "ugarchspec",  period = "auto", ...) {
    
    # X & Y
    others <- list(...)
    fit <- list()
    
    spec_type <- match.arg(spec_type, choices = c('ugarchspec'))
    
    if (!any(names(others) == "specs")){
        rlang::abort("In a multivariate garch regression, you have to specify a list 'specs' through set_engine")
    }
    
    specs <- others$specs
    
    others$specs <- NULL
    
    if (any(names(others) == "dcc_specs")){
        dcc_specs <- others$dcc_specs
        others$dcc_specs <- NULL
    }
    
    if (length(names(specs)) != dim(data)[2]){
        rlang::abort("The number of specifications passed through set_engine do not match the number of variables found in data.")
    }
    
    data_wider <- tidyr::pivot_wider(data, names_from = all.vars(formula)[3], values_from = all.vars(formula)[1])
    
    fit[["data"]] <- others[['data']] <- data_wider %>% 
        dplyr::select(-all.vars(formula)[2]) %>%
        as.data.frame()
    
    make_spec_call <- purrr::partial(parsnip::make_call, fun = spec_type, ns = "rugarch")
    
    specs <- purrr::map(specs, make_spec_call) %>% purrr::map(rlang::eval_tidy)
    
    dcc_specs[["uspec"]]<- others[['multispec']] <- rugarch::multispec(specs)
    
    fit[["fit"]] <- parsnip::make_call("multifit", "rugarch", others) %>% rlang::eval_tidy()
    
    fit[["spec"]] <- parsnip::make_call("dccspec", "rmgarch", dcc_specs) %>% rlang::eval_tidy()
    
    fit_dcc <- parsnip::make_call("dccfit", "rmgarch", fit) %>% rlang::eval_tidy()
    
    # RETURN
    new_modelgarch_bridge(
        class = "dcc_rmgarch_multi_fit_impl",
        
        # Models
        models = list(
            model_1 = fit_dcc
        ),
        
        extras = list(variables   = names(others[['data']]),
                      index       = all.vars(formula)[2],
                      names_from  = all.vars(formula)[3],
                      values_from = all.vars(formula)[1]),
        
        # Description - Convert arima model parameters to short description
        desc = 'DCC GARCH Multivariate Model'
    )
    
}

#' @export
print.dcc_rmgarch_multi_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}

# FIT - GARCH -----

#' Low-Level GARCH function for translating modeltime to forecast
#'
#' @param formula A dataframe of xreg (exogenous regressors)
#' @param data A numeric vector of values to fit
#' @param spec_type Must be ugarchspec
#' @param period auto
#' @param ... Additional arguments passed to `forecast::Arima`
#'
#' @export
#' @return A fitted model
cgarch_rmgarch_multi_fit_impl <- function(formula, data, spec_type = "ugarchspec",  period = "auto", ...) {

    # X & Y
    others <- list(...)
    fit <- list()

    spec_type <- match.arg(spec_type, choices = c('ugarchspec'))

    if (!any(names(others) == "specs")){
        rlang::abort("In a multivariate garch regression, you have to specify a list 'specs' through set_engine")
    }

    specs <- others$specs

    others$specs <- NULL

    if (any(names(others) == "c_specs")){
        c_specs <- others$c_specs
        others$c_specs <- NULL
    }

    if (length(names(specs)) != dim(data)[2]){
        rlang::abort("The number of specifications passed through set_engine do not match the number of variables found in data.")
    }

    data_wider <- tidyr::pivot_wider(data, names_from = all.vars(formula)[3], values_from = all.vars(formula)[1])

    fit[["data"]] <- others[['data']] <- data_wider %>%
        dplyr::select(-all.vars(formula)[2]) %>%
        as.data.frame()

    make_spec_call <- purrr::partial(parsnip::make_call, fun = spec_type, ns = "rugarch")

    specs <- purrr::map(specs, make_spec_call) %>% purrr::map(rlang::eval_tidy)

    c_specs[["uspec"]]<- others[['multispec']] <- rugarch::multispec(specs)

    fit[["fit"]] <- parsnip::make_call("multifit", "rugarch", others) %>% rlang::eval_tidy()

    fit[["spec"]] <- parsnip::make_call("cgarchspec", "rmgarch", c_specs) %>% rlang::eval_tidy()

    fit_c <- parsnip::make_call("cgarchfit", "rmgarch", fit) %>% rlang::eval_tidy()

    # RETURN
    new_modelgarch_bridge(
        class = "c_rmgarch_multi_fit_impl",

        # Models
        models = list(
            model_1 = fit_c
        ),

        extras = list(variables   = names(others[['data']]),
                      index       = all.vars(formula)[2],
                      names_from  = all.vars(formula)[3],
                      values_from = all.vars(formula)[1]),

        # Description - Convert arima model parameters to short description
        desc = 'Copula GARCH Multivariate Model'
    )

}

#' @export
print.cgarch_rmgarch_multi_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}


# FIT - GARCH -----

#' Low-Level GARCH function for translating modeltime to forecast
#'
#' @param formula A dataframe of xreg (exogenous regressors)
#' @param data A numeric vector of values to fit
#' @param spec_type NA
#' @param period auto
#' @param ... Additional arguments passed to `forecast::Arima`
#'
#' @export
#' @return A fitted model
gogarch_rmgarch_multi_fit_impl <- function(formula, data, spec_type = NULL,  period = "auto", ...) {
    
    # X & Y
    others <- list(...)
    
    if (any(names(others) == "gogarch_specs")){
        gogarch_specs <- others$gogarch_specs
        others$gogarch_specs <- NULL
    }
    
    data_wider <- tidyr::pivot_wider(data, names_from = all.vars(formula)[3], values_from = all.vars(formula)[1])
    
    others[['data']] <- data_wider %>%
        dplyr::select(-all.vars(formula)[2]) %>%
        as.data.frame()
    
    others[["spec"]] <- parsnip::make_call("gogarchspec", "rmgarch", gogarch_specs) %>% rlang::eval_tidy()
    
    fit_gogar <- parsnip::make_call("gogarchfit", "rmgarch", others) %>% rlang::eval_tidy()
    
    # RETURN
    new_modelgarch_bridge(
        class = "gogarch_rmgarch_multi_fit_impl",
        
        # Models
        models = list(
            model_1 = fit_gogar
        ),
        
        extras = list(variables   = names(others[['data']]),
                      index       = all.vars(formula)[2],
                      names_from  = all.vars(formula)[3],
                      values_from = all.vars(formula)[1]),
        
        # Description - Convert arima model parameters to short description
        desc = 'GO-GARCH GARCH Multivariate Model'
    )
    
}

#' @export
print.gogarch_rmgarch_multi_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}


#' @export
predict.rugarch_multi_fit_impl <- function(object, new_data, ...) {
    rugarch_multi_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for GARCH models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `stats::predict()`
#'
#' @export
#' @return A nested tibble
rugarch_multi_predict_impl <- function(object, new_data, ...) {
    
    # PREPARE INPUTS
    model       <- object$models$model_1
    variables   <- object$extras$variables
    index       <- object$extras$index
    names_from  <- object$extras$names_from
    values_from <- object$extras$values_from
    
    #New_data Setup
    
    if (!all(variables %in% names(new_data))){
        
        .new_data <- new_data %>% 
                    tidyr::pivot_wider(names_from = names_from, values_from = values_from) %>%
                    dplyr::select(dplyr::all_of(variables))
        
    }
    
    # PREDICTIONS
    
    preds_forecast <- rugarch::multiforecast(model, n.ahead = nrow(.new_data), ...) 
    
    preds_forecast_series <- preds_forecast@forecast %>% 
                      purrr::map(~.@forecast$seriesFor) %>% 
                      purrr::map(., delete_attr) %>%
                      purrr::map(., as.data.frame) %>%
                      purrr::reduce(dplyr::bind_cols) %>%
                      purrr::set_names(nm = variables) %>%
                      tidyr::pivot_longer(cols = variables[1]:variables[length(variables)]) %>%
                      dplyr::group_by(name) %>%
                      dplyr::mutate(!!index := new_data %>% dplyr::select({{ index }}) %>% unique()) %>%
                      dplyr::ungroup() %>%
                      dplyr::mutate(.name = "Series")
    
    preds_forecast_sigma <- preds_forecast@forecast %>% 
                        purrr::map(~.@forecast$sigmaFor) %>% 
                        purrr::map(., delete_attr) %>%
                        purrr::map(., as.data.frame) %>%
                        purrr::reduce(dplyr::bind_cols) %>%
                        purrr::set_names(nm = variables) %>%
                        tidyr::pivot_longer(cols = variables[1]:variables[length(variables)]) %>%
                        dplyr::group_by(name) %>%
                        dplyr::mutate(!!index := new_data %>% dplyr::select({{ index }}) %>% unique()) %>%
                        dplyr::ungroup() %>%
                        dplyr::mutate(.name = "Sigma")
    
    preds_forecast <- dplyr::bind_rows(preds_forecast_series, preds_forecast_sigma)

    
    return(preds_forecast)
    
}


#' @export
predict.dcc_rmgarch_multi_fit_impl <- function(object, new_data, ...) {
    dcc_rmgarch_multi_predict_impl(object, new_data, ...)
}


#' Bridge prediction function for GARCH models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `stats::predict()`
#'
#' @export
#' @return A nested tibble
dcc_rmgarch_multi_predict_impl <- function(object, new_data, ...) {
    
    # PREPARE INPUTS
    model       <- object$models$model_1
    variables   <- object$extras$variables
    index       <- object$extras$index
    names_from  <- object$extras$names_from
    values_from <- object$extras$values_from
    
    #New_data Setup
    
    if (!all(variables %in% names(new_data))){
        
        .new_data <- new_data %>% 
            tidyr::pivot_wider(names_from = names_from, values_from = values_from) %>%
            dplyr::select(dplyr::all_of(variables))
        
    }
    
    # PREDICTIONS
    
    preds_forecast <- rmgarch::dccforecast(model, n.ahead = nrow(.new_data), ...) 
    
    preds_forecast <- tibble::tibble(preds_forecast@mforecast) %>%
                      purrr::set_names(".pred") %>%
                      dplyr::mutate(.name = c("H", "R", "Q", "Rbar", "mu")) %>%
                      dplyr::relocate(".name", .before = ".pred")
    
    
    return(preds_forecast)
    
}



#' @export
predict.gogarch_rmgarch_multi_fit_impl <- function(object, new_data, ...) {
    gogarch_rmgarch_multi_predict_impl(object, new_data, ...)
}


#' Bridge prediction function for GARCH models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `stats::predict()`
#'
#' @export
#' @return A nested tibble
gogarch_rmgarch_multi_predict_impl <- function(object, new_data, ...) {
    
    # PREPARE INPUTS
    model       <- object$models$model_1
    variables   <- object$extras$variables
    index       <- object$extras$index
    names_from  <- object$extras$names_from
    values_from <- object$extras$values_from
    
    #New_data Setup
    
    if (!all(variables %in% names(new_data))){
        
        .new_data <- new_data %>% 
            tidyr::pivot_wider(names_from = names_from, values_from = values_from) %>%
            dplyr::select(dplyr::all_of(variables))
        
    }
    
    # PREDICTIONS
    
    preds_forecast <- rmgarch::gogarchforecast(model, n.ahead = nrow(.new_data), ...) 
    
    preds_forecast <- tibble::tibble(preds_forecast@mforecast) %>%
                      purrr::set_names(".pred") %>%
                      dplyr::mutate(.name = c("factor.sigmas", "mu", "A", "Y", "W", "K", "Kinv", "U", "arcoef", "garchcoef", "timer")) %>%
                      dplyr::relocate(".name", .before = ".pred")
    
    
    return(preds_forecast)
    
}



