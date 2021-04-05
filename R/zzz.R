

# IMPORTS ----
# - Parsnip gives user access to parsnip functions (fit.model_spec, set_engine)
# - rugarch gives user acces to ugarspech options

#' @import rugarch
#' @import parsnip
#' @import rmgarch
 
# ON LOAD ----

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
    # This defines the model database
    
    make_garch_reg()
    
    make_garch_reg_rugarch_rugarch()
    
    make_garch_multi_reg()
    
    make_garch_mutivariate_reg_rugarch_rugarch()
    
    make_garch_mutivariate_reg_rmgarch_dccrmgarch()
    
    make_garch_mutivariate_reg_rmgarch_crmgarch()
    
    make_garch_mutivariate_reg_rmgarch_gogarchrmgarch()
    
}

