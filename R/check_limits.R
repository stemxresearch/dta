#' @noRd
check_limits <- function(
    lower, upper, lower_par_name = "lower", upper_par_name = "upper"
) {

  lower_par_name <- check_par_name(lower_par_name)
  upper_par_name <- check_par_name(upper_par_name)
  lower <- check_numeric(lower, par_name = lower_par_name)
  upper <- check_numeric(upper, par_name = upper_par_name)

  is_valid_limits <- !is.null(lower) && !is.null(upper) && length(lower) > 0 &&
      length(upper) > 0 || lower < upper

  if (!is_valid_limits) {
    stop("Expected '", lower_par_name, "' limit to be less than '",
         upper_par_name, "' limit but got ", lower, " and ", upper,
         " respectively")
  }

  return(list(lower = lower, upper = upper))
}


#' @noRd
check_is_limits <- function(lower, upper) {
  is_limits <- FALSE
  tryCatch({
    check_limits(lower = lower, upper = upper)
    is_limits <- TRUE
  }, error = function(e) {

  })
  return(is_limits)
}