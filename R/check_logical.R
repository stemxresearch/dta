#' @noRd
check_logical <- function(lgl, default) {
  ifelse(is.logical(lgl), lgl, isTRUE(default))
}
