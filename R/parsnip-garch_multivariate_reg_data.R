

make_garch_multi_reg <- function() {
    parsnip::set_new_model("garch_multivariate_reg")
}


make_garch_mutivariate_reg_rugarch_rugarch <- function(){
    
    #### REGRESION
    model  = "garch_multivariate_reg"
    mode   = "regression"
    engine = "rugarch"
    
    parsnip::set_model_engine(model = model, mode = mode, eng = engine)
    parsnip::set_dependency(model = model, eng = engine, pkg = "rugarch")
    parsnip::set_dependency(model = model, eng = engine, pkg = "garchmodels")
    
    #Args
    
    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "type",
        original     = "spec_type",
        func         = list(pkg = "modelgarch", fun = "type"),
        has_submodel = FALSE
    )
    
    parsnip::set_encoding(
        model = model,
        eng   = engine,
        mode  = mode,
        options = list(
            predictor_indicators = "none",
            compute_intercept    = FALSE,
            remove_intercept     = FALSE,
            allow_sparse_x       = FALSE
        )
    )
    
    parsnip::set_fit(
        model = model,
        eng = engine,
        mode = mode,
        value = list(
            interface = "formula",
            protect = c("formula", "data"),
            func = c(fun = "rugarch_multi_fit_impl"),
            defaults = list()
        )
    )
    
    parsnip::set_pred(
        model  = model,
        eng    = engine,
        mode   = mode,
        type   = "numeric",
        value  = list(
            pre  = NULL,
            post = NULL,
            func = c(fun = "predict"),
            args = list(
                object = rlang::expr(object$fit),
                new_data = rlang::expr(new_data)
            )
        )
    )
    
    
}

make_garch_mutivariate_reg_rmgarch_dccrmgarch <- function(){
    
    #### REGRESION
    model  = "garch_multivariate_reg"
    mode   = "regression"
    engine = "dcc_rmgarch"
    
    parsnip::set_model_engine(model = model, mode = mode, eng = engine)
    parsnip::set_dependency(model = model, eng = engine, pkg = "rmgarch")
    parsnip::set_dependency(model = model, eng = engine, pkg = "garchmodels")
    
    #Args
    
    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "type",
        original     = "spec_type",
        func         = list(pkg = "modelgarch", fun = "type"),
        has_submodel = FALSE
    )
    
    parsnip::set_encoding(
        model = model,
        eng   = engine,
        mode  = mode,
        options = list(
            predictor_indicators = "none",
            compute_intercept    = FALSE,
            remove_intercept     = FALSE,
            allow_sparse_x       = FALSE
        )
    )
    
    parsnip::set_fit(
        model = model,
        eng = engine,
        mode = mode,
        value = list(
            interface = "formula",
            protect = c("formula", "data"),
            func = c(fun = "dcc_rmgarch_multi_fit_impl"),
            defaults = list()
        )
    )
    
    parsnip::set_pred(
        model  = model,
        eng    = engine,
        mode   = mode,
        type   = "numeric",
        value  = list(
            pre  = NULL,
            post = NULL,
            func = c(fun = "predict"),
            args = list(
                object = rlang::expr(object$fit),
                new_data = rlang::expr(new_data)
            )
        )
    )
    
    
}


make_garch_mutivariate_reg_rmgarch_crmgarch<- function(){
    
    #### REGRESION
    model  = "garch_multivariate_reg"
    mode   = "regression"
    engine = "c_rmgarch"
    
    parsnip::set_model_engine(model = model, mode = mode, eng = engine)
    parsnip::set_dependency(model = model, eng = engine, pkg = "rmgarch")
    parsnip::set_dependency(model = model, eng = engine, pkg = "garchmodels")
    
    #Args
    
    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "type",
        original     = "spec_type",
        func         = list(pkg = "modelgarch", fun = "type"),
        has_submodel = FALSE
    )
    
    parsnip::set_encoding(
        model = model,
        eng   = engine,
        mode  = mode,
        options = list(
            predictor_indicators = "none",
            compute_intercept    = FALSE,
            remove_intercept     = FALSE,
            allow_sparse_x       = FALSE
        )
    )
    
    parsnip::set_fit(
        model = model,
        eng = engine,
        mode = mode,
        value = list(
            interface = "formula",
            protect = c("formula", "data"),
            func = c(fun = "cgarch_rmgarch_multi_fit_impl"),
            defaults = list()
        )
    )
    
    parsnip::set_pred(
        model  = model,
        eng    = engine,
        mode   = mode,
        type   = "numeric",
        value  = list(
            pre  = NULL,
            post = NULL,
            func = c(fun = "predict"),
            args = list(
                object = rlang::expr(object$fit),
                new_data = rlang::expr(new_data)
            )
        )
    )
    
    
}


make_garch_mutivariate_reg_rmgarch_gogarchrmgarch<- function(){
    
    #### REGRESION
    model  = "garch_multivariate_reg"
    mode   = "regression"
    engine = "gogarch_rmgarch"
    
    parsnip::set_model_engine(model = model, mode = mode, eng = engine)
    parsnip::set_dependency(model = model, eng = engine, pkg = "rmgarch")
    parsnip::set_dependency(model = model, eng = engine, pkg = "garchmodels")
    
    #Args
    
    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "type",
        original     = "spec_type",
        func         = list(pkg = "modelgarch", fun = "type"),
        has_submodel = FALSE
    )
    
    parsnip::set_encoding(
        model = model,
        eng   = engine,
        mode  = mode,
        options = list(
            predictor_indicators = "none",
            compute_intercept    = FALSE,
            remove_intercept     = FALSE,
            allow_sparse_x       = FALSE
        )
    )
    
    parsnip::set_fit(
        model = model,
        eng = engine,
        mode = mode,
        value = list(
            interface = "formula",
            protect = c("formula", "data"),
            func = c(fun = "gogarch_rmgarch_multi_fit_impl"),
            defaults = list()
        )
    )
    
    parsnip::set_pred(
        model  = model,
        eng    = engine,
        mode   = mode,
        type   = "numeric",
        value  = list(
            pre  = NULL,
            post = NULL,
            func = c(fun = "predict"),
            args = list(
                object = rlang::expr(object$fit),
                new_data = rlang::expr(new_data)
            )
        )
    )
    
    
}






