#' Tuning Parameters for GARCHA Models
#'
#'
#' @inheritParams dials::Laplace
#'
#' @details
#' The main parameters for GARCHA models are:
#' 
#'  - `garch_order`: Integer with the garch order.
#'  - `arch_order`: Integer with the arch_order.
#'  - `mgarch_order`: Integer with the mgarch order.
#'  - `garch_t_student`: A boolean value to specify for a generalized t-student garch model.
#'  - `asymmetry`: a string value for the asymmetric function for an asymmetric GARCH process. By default the 
#'     value "none" for standard GARCH process. If "logit" a logistic function is used for asymmetry, and if 
#'     "exp" an exponential function is used.
#'  - `non_seasonal_ar`: The order of the non-seasonal auto-regressive (AR) terms.
#'  - `non_seasonal_ma`: The order of the non-seasonal moving average (MA) terms.
#'  - `markov_chains`: The number of markov chains.
#'  - `adapt_delta`: The thin of the jumps in a HMC method
#'  - `tree_depth`: Maximum depth of the trees
#'
#' @examples
#' non_seasonal_ar()
#'
#' non_seasonal_differences()
#'
#' non_seasonal_ma()
#'
#'
#' @name garch_params

#' @export
#' @return A parameter
#' @rdname garch_params
garch_order <- function(range = c(0L, 3L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(garch_order = "Garch Order"),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname garch_params
arch_order <- function(range = c(0L, 3L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(arch_order = "Arch Order"),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname garch_params
mgarch_order <- function(range = c(0L, 3L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(mgarch_order = "Mgarch Order"),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname garch_params
garch_t_student <- function() {
    dials::new_qual_param(
        type      = "logical",
        default   = FALSE,
        values    = c(FALSE, TRUE),
        label     = c(garch_t_student = "A boolean value to specify for a generalized t-student garch model."),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname garch_params
asymmetry <- function() {
    dials::new_qual_param(
        type      = "character",
        values    = c("none", "logit", "exp"),
        label     = c(asymmetry = "The asymmetric function for an asymmetric GARCH process."),
        finalize  = NULL
    )
}

#' #' @export
#' #' @rdname garch_params
#' non_seasonal_ar <- function(range = c(0L, 5L), trans = NULL) {
#'     dials::new_quant_param(
#'         type      = "integer",
#'         range     = range,
#'         inclusive = c(TRUE, TRUE),
#'         trans     = trans,
#'         label     = c(non_seasonal_ar = "Non-seasonal AR Term"),
#'         finalize  = NULL
#'     )
#' }
#' 
#' 
#' 
#' #' @export
#' #' @rdname garch_params
#' non_seasonal_ma <- function(range = c(0L, 5L), trans = NULL) {
#'     dials::new_quant_param(
#'         type      = "integer",
#'         range     = range,
#'         inclusive = c(TRUE, TRUE),
#'         trans     = trans,
#'         label     = c(non_seasonal_ma = "Non-seasonal MA Term"),
#'         finalize  = NULL
#'     )
#' }
#' 
#' 
#' 
#' #' @export
#' #' @rdname garch_params
#' markov_chains <- function(range = c(0L, 8L), trans = NULL) {
#'     dials::new_quant_param(
#'         type      = "integer",
#'         range     = range,
#'         inclusive = c(TRUE, TRUE),
#'         trans     = trans,
#'         label     = c(markov_chains = "Number of Markov Chains"),
#'         finalize  = NULL
#'     )
#' }
#' 
#' #' @export
#' #' @rdname garch_params
#' adapt_delta <- function(range = c(0, 1), trans = NULL) {
#'     dials::new_quant_param(
#'         type      = "double",
#'         range     = range,
#'         inclusive = c(TRUE, TRUE),
#'         trans     = trans,
#'         label     = c(adapt_delta = "The thin of the jumps in a HMC method"),
#'         finalize  = NULL
#'     )
#' }
#' 
#' #' @export
#' #' @rdname garch_params
#' tree_depth <- function(range = c(0L, 100L), trans = NULL) {
#'     dials::new_quant_param(
#'         type      = "integer",
#'         range     = range,
#'         inclusive = c(TRUE, TRUE),
#'         trans     = trans,
#'         label     = c(tree_depth = "Maximum depth of the trees"),
#'         finalize  = NULL
#'     )
#' }