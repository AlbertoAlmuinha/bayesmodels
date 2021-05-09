# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start


make_svm_reg <- function() {
    
    model <- "svm_reg"
    
    parsnip::set_new_model(model)
    parsnip::set_model_mode(model, "regression")
    
    # arima ----
    
    # * Model ----
    parsnip::set_model_engine(model, mode = "regression", eng = "stan")
    parsnip::set_dependency(model, "stan", "bayesforecast")
    parsnip::set_dependency(model, "stan", "bayesmodels")
    
    # * Args ----
    
    parsnip::set_model_arg(
        model        = model,
        eng          = "stan",
        parsnip      = "non_seasonal_ar",
        original     = "p",
        func         = list(pkg = "bayesmodels", fun = "non_seasonal_ar"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = model,
        eng          = "stan",
        parsnip      = "non_seasonal_ma",
        original     = "q",
        func         = list(pkg = "bayesmodels", fun = "non_seasonal_ma"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = model,
        eng          = "stan",
        parsnip      = "markov_chains",
        original     = "chains",
        func         = list(pkg = "bayesmodels", fun = "markov_chains"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = model,
        eng          = "stan",
        parsnip      = "chain_iter",
        original     = "iter",
        func         = list(pkg = "bayesmodels", fun = "chain_iter"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = model,
        eng          = "stan",
        parsnip      = "warmup_iter",
        original     = "warmup",
        func         = list(pkg = "bayesmodels", fun = "warmup_iter"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = model,
        eng          = "stan",
        parsnip      = "adapt_delta",
        original     = "adapt.delta",
        func         = list(pkg = "bayesmodels", fun = "adapt_delta"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = model,
        eng          = "stan",
        parsnip      = "tree_depth",
        original     = "tree.depth",
        func         = list(pkg = "bayesmodels", fun = "tree_depth"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = model,
        eng          = "stan",
        parsnip      = "pred_seed",
        original     = "seed",
        func         = list(pkg = "bayesmodels", fun = "pred_seed"),
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
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "svm_stan_fit_impl"),
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