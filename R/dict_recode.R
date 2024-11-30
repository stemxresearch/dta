#' A dictionary with ariable value mappings with labels and ordered status.
#'
#' A dictionary containing value mappings for various variables, along with
#' their corresponding labels and whether the values are ordered.
#'
#' @format A tibble with 50 rows and 4 variables:
#' \describe{
#'   \item{names}{\code{character}: The variable name.}
#'   \item{values}{\code{numeric}: The numeric value assigned to each label.}
#'   \item{labels}{\code{character}: The label corresponding to each numeric
#'   value.}
#'   \item{is_ordered}{\code{numeric}: Indicates whether the variable is
#'   ordered. A value of 1 indicates ordered, 0 indicates unordered.}
#' }
#'
#' @usage
#' data("dict_recode")
#'
#' @examples
#' dta_gtable(dict_recode)
#'
#' @source Simulated data for demonstration purposes.
"dict_recode"