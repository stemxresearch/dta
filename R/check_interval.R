#' @noRd
check_interval <- function(h, lower, upper) {
  h <- check_numeric(num = h, min = 1e-16, par_name = "h")
  lower <- check_numeric(num = lower, par_name = "lower")
  upper <- check_numeric(num = upper, par_name = "upper")
  
  check_limits(
    lower = min, upper = max, lower_par_name = "lower", upper_par_name = "upper"
  )

  if (h > abs(upper - lower)) {
    stop("Expected 'h' to be less than or equal to |upper - lower| = ",
         abs(upper - lower), " but got: h = ", h)
  }

  return(list(lower = lower, upper = upper, h = h))
}


#' @noRd
check_is_interval <- function(h, lower, upper) {
  is_interval <- FALSE
  tryCatch({
    check_interval(h = h, lower = lower, upper = upper)
    is_interval <- TRUE
  }, error = function(e) {

  })
  
  return(is_interval)
}