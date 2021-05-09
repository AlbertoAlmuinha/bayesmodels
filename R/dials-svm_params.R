#' Tuning Parameters for Stochastic Volatility Models
#'
#'
#' @inheritParams dials::Laplace
#'
#' @details
#' The main parameters for Stochastic Volatility models are:
#'
#'  - `non_seasonal_ar`: The order of the non-seasonal auto-regressive (AR) terms.
#'  - `non_seasonal_ma`: The order of the non-seasonal moving average (MA) terms.
#'  - `markov_chains`: The number of markov chains.
#'  - `adapt_delta`: The thin of the jumps in a HMC method
#'  - `tree_depth`: Maximum depth of the trees
#'
#' @examples
#' non_seasonal_ar()
#'
#' non_seasonal_ma()
#'
#' @name svm_params


#' #' @export
#' #' @rdname svm_params
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
#' #' @export
#' #' @rdname svm_params
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
#' #' @export
#' #' @rdname svm_params
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
#' #' @rdname svm_params
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
#' #' @rdname svm_params
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