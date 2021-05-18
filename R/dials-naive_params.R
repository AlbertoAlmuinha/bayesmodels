#' Tuning Parameters for Random Walk Models
#'
#'
#' @inheritParams dials::Laplace
#'
#' @details
#' The main parameters for Random Walk Models are:
#' 
#'  - `seasonal_random_walk`: A boolean value for select a seasonal random walk instead.
#'  - `markov_chains`: The number of markov chains.
#'  - `adapt_delta`: The thin of the jumps in a HMC method
#'  - `tree_depth`: Maximum depth of the trees
#'
#'
#'
#' @name naive_params

#' @export
#' @return A parameter
#' @rdname naive_params
seasonal_random_walk <- function() {
    dials::new_qual_param(
        type      = "logical",
        default   = FALSE,
        values    = c(FALSE, TRUE),
        label     = c(seasonal_random_walk = "A boolean value for select a seasonal random walk instead."),
        finalize  = NULL
    )
}


#' #' @export
#' #' @rdname naive_params
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
#' #' @rdname naive_params
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
#' #' @rdname naive_params
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