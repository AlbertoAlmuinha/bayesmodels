# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start


make_adaptive_spline_reg <- function() {
    
    model <- "adaptive_spline"
    
    parsnip::set_new_model(model)
    parsnip::set_model_mode(model, "regression")
    
    # arima ----
    
    # * Model ----
    parsnip::set_model_engine(model, mode = "regression", eng = "stan")
    parsnip::set_dependency(model, "stan", "BASS")
    parsnip::set_dependency(model, "stan", "bayesmodels")
    
    # * Args ----
    
    parsnip::set_model_arg(
        model        = model,
        eng          = "stan",
        parsnip      = "splines_degree",
        original     = "degree",
        func         = list(pkg = "bayesmodels", fun = "splines_degree"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = model,
        eng          = "stan",
        parsnip      = "max_degree",
        original     = "maxInt",
        func         = list(pkg = "bayesmodels", fun = "max_degree"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = model,
        eng          = "stan",
        parsnip      = "max_categorical_degree",
        original     = "maxInt.cat",
        func         = list(pkg = "bayesmodels", fun = "max_categorical_degree"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = model,
        eng          = "stan",
        parsnip      = "min_basis_points",
        original     = "npart",
        func         = list(pkg = "bayesmodels", fun = "min_basis_points"),
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
            func      = c(fun = "adaptive_spline_stan_fit_impl"),
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

#nocov end