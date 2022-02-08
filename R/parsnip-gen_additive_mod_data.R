# nocov start

make_gen_additive_reg <- function() {
    parsnip::set_new_model("gen_additive_reg")
    parsnip::set_model_mode("gen_additive_reg", "regression")
}

make_gen_additive_reg_stan <- function() {
    
    #### REGRESION
    model  = "gen_additive_reg"
    mode   = "regression"
    engine = "stan"
    
    parsnip::set_model_engine(model = model, mode = mode, eng = engine)
    parsnip::set_dependency(model = model, eng = engine, pkg = "brms")
    parsnip::set_dependency(model = model, eng = engine, pkg = "bayesmodels")
    
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
            func = c(fun = "gen_additive_stan_fit_impl"),
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
            post = NULL, #function(x, object) res<-tibble::as_tibble(x) %>% dplyr::pull(1) %>% as.numeric(),
            func = c(fun = "predict"),
            args = list(
                object = rlang::expr(object$fit),
                new_data = rlang::expr(new_data)
            )
        )
    )
    
}

# nocov end