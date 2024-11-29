#' @noRd
check_digits_from_tol <- function(vec, tol = 1e-16) {
  if (is.null(tol) || !is.numeric(tol) || length(tol) != 1) {
    if (tol < 0 || tol > 1) {
      tol <- 1e-16
    }
    digits <- max(
      sapply(vec, function(number) {
        if (grepl("\\.", as.character(number))) {
          nchar(strsplit(as.character(number), "\\.")[[1]][2])
        } else {
          0
        }
      })
    )
  } else {
    digits <- ceiling(-log10(tol))
  }
  return(digits)
}


#' @noRd
check_digits <- function(digits) {
  digits <- check_scalar_integer(num = digits, par_name = "digits")
  ifelse(is.null(digits), 14, digits)
}
