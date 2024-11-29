#' @noRd
check_vector_or_matrix <- function(
    vmat, allow_scalar = TRUE, par_name = "vmat") {
  
  allow_scalar <- check_logical(allow_scalar, default = TRUE)
  n <- ifelse(allow_scalar, 1, 2)
  is_vector_or_matrix <- (is.vector(vmat) && length(vmat) >= n) ||
    is.matrix(vmat)

  if (!is_vector_or_matrix) {
    par_name <- check_par_name(par_name)
    n <- ifelse(allow_scalar, 1, 2)
    s <- check_singular_plural(n = n)
    stop("Expected '", par_name, "' to be a vector with at least ", n,
         " element", s, " or a matrix but got ", check_class(vmat))
  }

  return(vmat)
}


#' @noRd
check_is_vector_or_matrix <- function(vmat, allow_scalar = TRUE) {
  is_vector_or_matrix <- FALSE
  tryCatch({
    check_vector_or_matrix(vmat = vmat, allow_scalar = allow_scalar)
    is_vector_or_matrix <- TRUE
  }, error = function(e) {
    
  })
  
  return(is_vector_or_matrix)
}
