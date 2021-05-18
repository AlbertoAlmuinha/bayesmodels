#' Tuning Parameters for Exponential Smoothing Models
#'
#'
#' @inheritParams dials::Laplace
#'
#' @details
#' The main parameters for Exponential Smoothing models are:
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
#' @name exponential_smoothing_params

#' @export
#' @return A parameter
#' @rdname exponential_smoothing_params
seasonality_type <- function() {
    dials::new_qual_param(
        type      = "character",
        values    = c("multiplicative", "generalized"),
        label     = c(seasonality_type = "The latter seasonality generalizes additive and multiplicative seasonality types"),
        finalize  = NULL
    )
}


#' @export
#' @return A parameter
#' @rdname exponential_smoothing_params
method <- function() {
    dials::new_qual_param(
        type      = "character",
        values    = c("HW", "seasAvg", "HW_sAvg"),
        label     = c(method = "Here, HW follows Holt-Winters approach. seasAvg calculates level as a smoothed average of the last seasonality number of points (or seasonality2 of them for the dual seasonality model), and HW_sAvg is an weighted average of HW and seasAvg methods"),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname exponential_smoothing_params
error_method <- function() {
    dials::new_qual_param(
        type      = "character",
        values    = c("std", "innov"),
        label     = c(error_method = "s
                      Function providing size of the error"),
        finalize  = NULL
    )
}
