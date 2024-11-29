#' @noRd
check_ncols <- function(n, min = 1, max = Inf, allow_null = FALSE) {
  min <- check_numeric(min, par_name = "min")
  max <- check_numeric(max, par_name = "max")
  n <- check_numeric(
    num = n, min = min, max = max, is_integer = TRUE,
    allow_null = allow_null, par_name = "ncols"
  )
  return(n)
}
