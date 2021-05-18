#' Tuning Parameters for SARIMA Models
#'
#'
#' @inheritParams dials::Laplace
#'
#' @details
#' The main parameters for SARIMA models are:
#'
#'  - `non_seasonal_ar`: The order of the non-seasonal auto-regressive (AR) terms.
#'  - `non_seasonal_differences`: The order of integration for non-seasonal differencing.
#'  - `non_seasonal_ma`: The order of the non-seasonal moving average (MA) terms.
#'  - `seasonal_ar`: The order of the seasonal auto-regressive (SAR) terms.
#'  - `seasonal_differences`: The order of integration for seasonal differencing.
#'  - `seasonal_ma`: The order of the seasonal moving average (SMA) terms.
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
#' @name sarima_params


#' @export
#' @return A parameter
#' @rdname sarima_params
non_seasonal_ar <- function(range = c(0L, 5L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(non_seasonal_ar = "Non-seasonal AR Term"),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname sarima_params
non_seasonal_differences <- function(range = c(0L, 2L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(non_seasonal_differences = "Non-seasonal Differencing Term"),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname sarima_params
non_seasonal_ma <- function(range = c(0L, 5L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(non_seasonal_ma = "Non-seasonal MA Term"),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname sarima_params
seasonal_ar <- function(range = c(0L, 2L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(seasonal_ar = "Seasonal AR Term"),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname sarima_params
seasonal_differences <- function(range = c(0L, 1L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(seasonal_differences = "Seasonal Differencing Term"),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname sarima_params
seasonal_ma <- function(range = c(0L, 2L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(seasonal_ma = "Seasonal MA Term"),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname sarima_params
markov_chains <- function(range = c(0L, 8L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(markov_chains = "Number of Markov Chains"),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname sarima_params
adapt_delta <- function(range = c(0, 1), trans = NULL) {
    dials::new_quant_param(
        type      = "double",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(adapt_delta = "The thin of the jumps in a HMC method"),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname sarima_params
tree_depth <- function(range = c(0L, 100L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(tree_depth = "Maximum depth of the trees"),
        finalize  = NULL
    )
}