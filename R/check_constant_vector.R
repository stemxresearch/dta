#' @noRd
check_constant_vector <- function(vec, tol = 1e-16, par_name = "vec") {
  
  par_name <- check_par_name(par_name)
  vec <- check_vector(
    vec = vec, n = NULL, inequality = ">=", par_name = par_name
  )
  tol <- check_numeric(
    num = tol, min = 0, max = 1, boundary = "exclusive", par_name = par_name
  )

  vec_rounded <- round(vec, digits = -log10(tol))
  is_constant <- all(vec_rounded == vec_rounded[1])

  if (!is_constant) {
    stop("Expected '", par_name, "' to be a constant vector but got a vector",
         " with different values.")
  }
  return(vec)
}


#' @noRd
check_is_constant_vector <- function(vec, tol = 1e-16, par_name = "vec") {
  is_constant_vector <- FALSE
  tryCatch({
    check_constant_vector(vec = vec, tol = tol)
    is_constant_vector <- TRUE
  }, error = function(e) {
    
  })
  return(is_constant_vector)
}
