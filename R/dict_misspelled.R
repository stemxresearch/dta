#' A dictionary with variable mappings for data standardization.
#'
#' A dataset containing mappings of old values to new values for certain
#' specified variables in a dataset.
#'
#' @format A tibble with 29 rows and 3 variables:
#' \describe{
#'   \item{variable}{\code{character}: The variable for which old and new
#'   values are mapped.}
#'   \item{old}{\code{character}: The old value to be replaced.}
#'   \item{new}{\code{character}: The new value to replace the old value.}
#' }
#'
#' @usage
#' data("dict_misspelled")
#'
#' @examples
#' dta_gtable(dict_misspelled)
#'
#' @source Simulated data for demonstration purposes.
"dict_misspelled"