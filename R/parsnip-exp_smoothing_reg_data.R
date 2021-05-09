# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start


make_exponential_smoothing_reg <- function() {
    
    parsnip::set_new_model("exponential_smoothing")
    parsnip::set_model_mode("exponential_smoothing", "regression")
    
    # arima ----
    
    # * Model ----
    parsnip::set_model_engine("exponential_smoothing", mode = "regression", eng = "stan")
    parsnip::set_dependency("exponential_smoothing", "stan", "Rlgt")
    parsnip::set_dependency("exponential_smoothing", "stan", "bayesmodels")

    
    parsnip::set_model_arg(
        model        = "exponential_smoothing",
        eng          = "stan",
        parsnip      = "seasonality",
        original     = "seasonality",
        func         = list(pkg = "bayesmodels", fun = "seasonality"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "exponential_smoothing",
        eng          = "stan",
        parsnip      = "second_seasonality",
        original     = "seasonality2",
        func         = list(pkg = "bayesmodels", fun = "second_seasonality"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "exponential_smoothing",
        eng          = "stan",
        parsnip      = "seasonality_type",
        original     = "seasonality.type",
        func         = list(pkg = "bayesmodels", fun = "seasonality_type"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "exponential_smoothing",
        eng          = "stan",
        parsnip      = "method",
        original     = "level.method",
        func         = list(pkg = "bayesmodels", fun = "method"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "exponential_smoothing",
        eng          = "stan",
        parsnip      = "error_method",
        original     = "error.size.method",
        func         = list(pkg = "bayesmodels", fun = "error_method"),
        has_submodel = FALSE
    )
    
    
    # * Encoding ----
    parsnip::set_encoding(
        model   = "exponential_smoothing",
        eng     = "stan",
        mode    = "regression",
        options = list(
            predictor_indicators = "none",
            compute_intercept    = FALSE,
            remove_intercept     = FALSE,
            allow_sparse_x       = FALSE
        )
    )
    
    # * Fit ----
    parsnip::set_fit(
        model         = "exponential_smoothing",
        eng           = "stan",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "exp_smoothing_stan_fit_impl"),
            defaults  = list()
        )
    )
    
    # * Predict ----
    parsnip::set_pred(
        model         = "exponential_smoothing",
        eng           = "stan",
        mode          = "regression",
        type          = "numeric",
        value         = list(
            pre       = NULL,
            post      = NULL,
            func      = c(fun = "predict"),
            args      =
                list(
                    object    = rlang::expr(object$fit),
                    new_data  = rlang::expr(new_data)
                )
        )
    )
}

# nocov end