

# IMPORTS ----
# - Parsnip gives user access to parsnip functions (fit.model_spec, set_engine)
# - bsts gives user acces to AddLocalLinearTrend functions etc

#' @import bayesforecast
#' @import bsts
#' @import parsnip
 
# ON LOAD ----

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
    # This defines the model database
    
    make_sarima_reg()
    
    make_garch_reg()
    
    make_svm_reg()
    
    make_ssm_reg()
    
    make_random_walk_reg()
    
    make_gen_additive_reg()
    make_gen_additive_reg_stan()
    
    make_bsr_reg()
    
    make_exponential_smoothing_reg()
    
    make_adaptive_spline_reg()
    
}

