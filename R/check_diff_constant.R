#' @noRd
check_diff_constant <- function(vec, n = 2, tol = 1e-16, par_name = "vec") {
  
  par_name <- check_par_name(par_name)
  vec <- check_vector(vec, n = 2, inequality = ">=", par_name = "vec")
  n <- check_numeric(
    n, min = 1, max = length(vec), is_integer = TRUE, par_name = "n"
  )
  tol <- check_numeric(
    tol, min = 0, max = 1, boundary = "exclusive", allow_null = TRUE
  )

  digits <- check_digits_from_tol(vec = vec, tol = tol)
  vec_rounded = round(diff(vec, differences = n), digits = digits)
  is_diff_constant <- all(vec_rounded == vec_rounded[1])

  if (!is_diff_constant) {
    par_name <- check_par_name(par_name)
    stop("Difference between elements of '", par_name, "' must be constant,",
         " but got ", check_stop(vec))
  }

  return(vec)
}


#' @noRd
check_is_diff_constant <- function(vec, n = 2, tol = 1e-16) {
  is_diff_constant <- FALSE
  tryCatch({
    check_diff_constant(vec = vec, n = n, tol = tol)
    is_diff_constant <- TRUE
  }, error = function(e) {
    
  })
  return(is_diff_constant)  
}
