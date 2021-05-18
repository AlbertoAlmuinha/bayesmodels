#' Tuning Parameters for Adaptive Splines Surface Models
#'
#'
#' @inheritParams dials::Laplace
#'
#' @details
#' The main parameters for Adaptive Splines Surface models are:
#'
#'  - `splines_degree`: degree of splines. Stability should be examined for anything other than 1.
#'  - `max_degree`: integer for maximum degree of interaction in spline basis functions. 
#'  - `max_categorical_degree`: (categorical input only) integer for maximum degree of interaction of categorical inputs.
#'  - `min_basis_points`: minimum number of non-zero points in a basis function
#'
#' @examples
#' splines_degree()
#'
#' max_degree()
#'
#' min_basis_points()
#'
#'
#' @name adaptive_splines_params


#' @export
#' @return A parameter
#' @rdname adaptive_splines_params
splines_degree <- function(range = c(0L, 5L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(splines_degree = "Degree of splines"),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname adaptive_splines_params
max_degree <- function(range = c(0L, 5L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(max_degree = "Maximum degree of interaction in spline basis functions"),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname adaptive_splines_params
max_categorical_degree <- function(range = c(0L, 5L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(max_categorical_degree = "Maximum degree of interaction of categorical inputs"),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname adaptive_splines_params
min_basis_points <- function(range = c(0L, 1000L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(min_basis_points = "Minimum number of non-zero points in a basis function"),
        finalize  = NULL
    )
}
