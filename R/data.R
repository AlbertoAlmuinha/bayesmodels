# rIBM ----

#' The IBM's Daily Returns
#'
#'
#' @format
#' A `tibble` with 3523 rows and 2 variables:
#'
#'  - `date` Date. Timestamp information. Daily format.
#'  - `daily_returns` Numeric. Value at the corresponding timestamp.
#'
#' @examples
#' rIBM
#'
#'
"rIBM"


# rX_longer ----

#' IBM, Google and BP's daily returns in long format
#'
#'
#' @format
#' A `tibble` with 8550 rows and 3 variables:
#' 
#'  - `id` Factor. Unique series identifier
#'  - `date` Date. Timestamp information. Daily format.
#'  - `value` Numeric. Value at the corresponding timestamp.
#'
#' @details
#'
#' ``` {r eval = FALSE}
#' library(timetk)
#' m750_splits <- time_series_split(m750, assess = "2 years", cumulative = TRUE)
#' ```
#'
#' @examples
#'
#' rX_longer
#'
"rX_longer"