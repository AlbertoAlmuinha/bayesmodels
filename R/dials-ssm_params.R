#' Tuning Parameters for Additive Linear State Space Regression Models
#'
#'
#' @inheritParams dials::Laplace
#'
#' @details
#' The main parameters for Additive Linear State Space Regression Models are:
#' 
#'  - `trend_model`: A boolean value to specify a trend local level model.
#'  - `damped_model`: A boolean value to specify a damped trend local level model.
#'  - `seasonal_model`: A boolean value to specify a seasonal trend local level model.
#'  - `markov_chains`: The number of markov chains.
#'  - `adapt_delta`: The thin of the jumps in a HMC method
#'  - `tree_depth`: Maximum depth of the trees
#'
#' @examples
#'
#' damped_model()
#'
#' seasonal_model()
#'
#'
#' @name ssm_params

#' @
#' @return A parameter
#' @rdname ssm_params
trend_model <- function() {
    dials::new_qual_param(
        type      = "logical",
        default   = FALSE,
        values    = c(FALSE, TRUE),
        label     = c(trend_model = "a boolean value to specify a trend local level model."),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname ssm_params
damped_model <- function() {
    dials::new_qual_param(
        type      = "logical",
        default   = FALSE,
        values    = c(FALSE, TRUE),
        label     = c(damped_model = "a boolean value to specify a damped trend local level model."),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname ssm_params
seasonal_model <- function() {
    dials::new_qual_param(
        type      = "logical",
        default   = FALSE,
        values    = c(FALSE, TRUE),
        label     = c(seasonal_model = "a boolean value to specify a seasonal trend local level model."),
        finalize  = NULL
    )
}

#' #' @export
#' #' @rdname ssm_params
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
#' #' @rdname ssm_params
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
#' #' @rdname ssm_params
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