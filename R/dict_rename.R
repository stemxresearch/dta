#' A dictionary for variable name changes.
#'
#' A dictionary containing mappings of old variable names to their
#' corresponding new names.
#'
#' @format A tibble with 9 rows and 2 variables:
#' \describe{
#'   \item{old}{\code{character}: The old variable name.}
#'   \item{new}{\code{character}: The new variable name that the old one has
#'   been changed to.}
#' }
#'
#' @usage
#' data("dict_rename")
#'
#' @examples
#' dta_gtable(dict_rename)
#'
#' @source Simulated data for variable name mapping.
"dict_rename"
