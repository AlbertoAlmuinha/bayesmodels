

make_garch_reg <- function() {
    parsnip::set_new_model("garch_reg")
}


make_garch_reg_rugarch_rugarch <- function(){
    
    #### REGRESION
    model  = "garch_reg"
    mode   = "regression"
    engine = "rugarch"
    
    parsnip::set_model_engine(model = model, mode = mode, eng = engine)
    parsnip::set_dependency(model = model, eng = engine, pkg = "rugarch")
    parsnip::set_dependency(model = model, eng = engine, pkg = "garchmodels")
    
    #Args
    
    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "arch_order",
        original     = "a",
        func         = list(pkg = "modelgarch", fun = "arch_order"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "garch_order",
        original     = "g",
        func         = list(pkg = "modelgarch", fun = "garch_order"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "ar_order",
        original     = "ar",
        func         = list(pkg = "modelgarch", fun = "ar_order"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "ma_order",
        original     = "ma",
        func         = list(pkg = "modelgarch", fun = "ma_order"),
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
            func = c(fun = "rugarch_fit_impl"),
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


# make_garch_reg_tseries_garch <- function() {
#     
#     #### REGRESION
#     model  = "garch_reg"
#     mode   = "regression"
#     engine = "garch"
#     
#     parsnip::set_model_engine(model = model, mode = mode, eng = engine)
#     parsnip::set_dependency(model = model, eng = engine, pkg = "tseries")
#     parsnip::set_dependency(model = model, eng = engine, pkg = "garchmodels")
#     
#     #Args
#     
#     parsnip::set_model_arg(
#         model        = model,
#         eng          = engine,
#         parsnip      = "arch_order",
#         original     = "a",
#         func         = list(pkg = "modelgarch", fun = "arch_order"),
#         has_submodel = FALSE
#     )
#     
#     parsnip::set_model_arg(
#         model        = model,
#         eng          = engine,
#         parsnip      = "garch_order",
#         original     = "g",
#         func         = list(pkg = "modelgarch", fun = "garch_order"),
#         has_submodel = FALSE
#     )
#     
#     parsnip::set_model_arg(
#         model        = model,
#         eng          = engine,
#         parsnip      = "ar_order",
#         original     = "ar_no_apply",
#         func         = list(pkg = "modelgarch", fun = "ar_order"),
#         has_submodel = FALSE
#     )
#     
#     parsnip::set_model_arg(
#         model        = model,
#         eng          = engine,
#         parsnip      = "ma_order",
#         original     = "ma_no_apply",
#         func         = list(pkg = "modelgarch", fun = "ma_order"),
#         has_submodel = FALSE
#     )
#     
#     
#     parsnip::set_encoding(
#         model = model,
#         eng   = engine,
#         mode  = mode,
#         options = list(
#             predictor_indicators = "none",
#             compute_intercept    = FALSE,
#             remove_intercept     = FALSE,
#             allow_sparse_x       = FALSE
#         )
#     )
#     
#     parsnip::set_fit(
#         model = model,
#        eng = engine,
#         mode = mode,
#         value = list(
#             interface = "formula",
#             protect = c("formula", "data"),
#             func = c(fun = "garch_fit_impl"),
#             defaults = list()
#         )
#     )
#     
#     parsnip::set_pred(
#         model  = model,
#         eng    = engine,
#         mode   = mode,
#         type   = "numeric",
#         value  = list(
#             pre  = NULL,
#             post = NULL,
#             func = c(fun = "predict"),
#             args = list(
#                 object = rlang::expr(object$fit),
#                 new_data = rlang::expr(new_data)
#             )
#         )
#     )
#     
#     
#     
# }