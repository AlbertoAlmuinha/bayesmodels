#' Tuning Parameters for Univariate Garch Models
#'
#'
#' @inheritParams dials::Laplace
#'
#' @details
#' The main parameters for Univariate Garch models are:
#'
#'  - `arch_order`: The order corresponding to the ARCH part.
#'  - `garch_order`: The order corresponding to the GARCH part.
#'  - `ar_order`: The order of the non-seasonal auto-regressive (AR) terms.
#'  - `ma_order`: The order of the non-seasonal moving average (MA) terms.
#'
#' @examples
#' arch_order()
#'
#' garch_order()
#'
#' ar_order()
#' 
#' ma_order()
#'
#'
#' @name garch_params


#' @export
#' @rdname garch_params
arch_order <- function(range = c(0L, 3L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(arch_order = "ARCH Order Part"),
        finalize  = NULL
    )
}

#' @export
#' @rdname garch_params
garch_order <- function(range = c(0L, 3L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(garch_order = "GARCH Order Part"),
        finalize  = NULL
    )
}

#' @export
#' @rdname garch_params
ar_order <- function(range = c(0L, 5L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(ar_order = "Non-seasonal AR Term"),
        finalize  = NULL
    )
}

#' @export
#' @rdname garch_params
ma_order <- function(range = c(0L, 5L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(ma_order = "Non-seasonal MA Term"),
        finalize  = NULL
    )
}
