

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

.onAttach <- function(libname, pkgname) {
    
    bsu_rule_color <- "#2c3e50"
    bsu_main_color <- "#1f78b4"
    
    # Check Theme: If Dark, Update Colors
    tryCatch({
        if (rstudioapi::isAvailable()) {
            theme <- rstudioapi::getThemeInfo()
            if (is.null(theme)) {
                bsu_rule_color <- "#2c3e50"
                bsu_main_color <- "#1f78b4"
            }
            if (theme$dark) {
                bsu_rule_color <- "#7FD2FF"
                bsu_main_color <- "#18bc9c"
            }
        }
    }, error = function(e) {
        bsu_rule_color <- "#2c3e50"
        bsu_main_color <- "#1f78b4"
    }, finally = {
        bsu_main <- crayon::make_style(bsu_main_color)
        
        msg <- paste0(
            cli::rule(left = "Welcome to bayesmodels", col = bsu_rule_color, line = 2),
            bsu_main('\nIf you are interested in time series, maybe you would like to check my other packages: garchmodels and boostime\n'),
            bsu_main('</> If you find this package useful, please leave a star: https://github.com/AlbertoAlmuinha/bayesmodels </>')
        )
        
        packageStartupMessage(msg)
    })
    
}