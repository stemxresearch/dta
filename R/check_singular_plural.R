#' @noRd
check_singular_plural <- function(n, singular = "", plural = "s") {
  # do not use `check...()` to avoid recursion
  stopifnot(is.numeric(n), length(n) == 1, n %% 1 == 0, n >= 0)
  stopifnot(is.character(singular), length(singular) == 1)
  stopifnot(is.character(plural), length(plural) == 1)
  ifelse(n > 1, plural, singular)
}
