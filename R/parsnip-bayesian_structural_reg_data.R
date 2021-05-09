# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start


make_bsr_reg <- function() {
    
    model <- "bayesian_structural_reg"
    
    parsnip::set_new_model(model)
    parsnip::set_model_mode(model, "regression")
    
    # arima ----
    
    # * Model ----
    parsnip::set_model_engine(model, mode = "regression", eng = "stan")
    parsnip::set_dependency(model, "stan", "bsts")
    parsnip::set_dependency(model, "stan", "bayesmodels")
    
    # * Args ----
    
    parsnip::set_model_arg(
        model        = model,
        eng          = "stan",
        parsnip      = "distribution",
        original     = "family",
        func         = list(pkg = "bayesmodels", fun = "distribution"),
        has_submodel = FALSE
    )
    
    # * Encoding ----
    parsnip::set_encoding(
        model   = model,
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
        model         = model,
        eng           = "stan",
        mode          = "regression",
        value         = list(
            interface = "formula",
            protect   = c("formula", "data"),
            func      = c(fun = "bayesian_structural_stan_fit_impl"),
            defaults  = list()
        )
    )
    
    # * Predict ----
    parsnip::set_pred(
        model         = model,
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