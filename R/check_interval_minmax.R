#' @noRd
check_interval_minmax <- function(num, min, max, par_name = "num") {

  par_name <- check_par_name(par_name)
  num <- check_numeric(num, par_name = par_name)
  min <- check_numeric(min, par_name = "min")
  max <- check_numeric(max, par_name = "max")

  check_limits(
    lower = min, upper = max, lower_par_name = "min", upper_par_name = "max"
  )

  is_in_interval <- num >= min && num <= max

  if (is_in_interval) {
    stop("Expected '", par_name, "' to be between ", min, " and ", max,
         " but got ", check_stop(num))
  }

  return(num)
}


#' @noRd
check_is_interval_minmax <- function(num, min, max) {
  is_interval_minmax <- FALSE
  tryCatch({
    check_interval_minmax(num = num, min = min, max = max)
    is_interval_minmax <- TRUE
  }, error = function(e) {

  })
  return(is_interval_minmax)
}


#' @noRd
check_interval_vector <- function(
    vec, min, max, action = c("warning", "stop", "none"), par_name = "vec"
) {

  par_name <- check_par_name(par_name)
  vec <- check_vector(vec, n = 2, inequality = ">=", par_name = "vec")
  min <- check_numeric(min, par_name = "min")
  max <- check_numeric(max, par_name = "max")
  check_limits(
    lower = min, upper = max, lower_par_name = "min", upper_par_name = "max"
  )
  action <- match.arg(action)[1]

  is_in_interval <- vec >= min && vec <= max

  if (!any(is_in_interval)) {
    values_outside_interval <- vec[!is_in_interval]
    s <- check_singular_plural(n = length(values_outside_interval))
    is_are <- check_singular_plural(
      n = length(values_outside_interval), singular = "is", plural = "are"
    )
    if (action == "warning") {
      warning("The value", s, " ", paste0(values_outside_interval, collapse = ", "),
              " fall outside the interval [", min, ", ", max, "]")
    } else if (action == "stop") {
      stop("Expected all values of 'vec' to be between ", min, " and ", max,
           ". The value", s, ": ", paste0(values_outside_interval, collapse = ", "),
           " fall outside the this interval")
    }
  }

  return(vec)
}


#' @noRd
check_is_interval_vector <- function(
    vec, min, max, action = c("warning", "stop", "none")
) {
  is_interval_vector <- FALSE
  tryCatch({
    check_interval_vector(vec = vec, min = min, max = max, action = action)
    is_interval_vector <- TRUE
  }, error = function(e) {

  })
  
  return(is_interval_vector)
}