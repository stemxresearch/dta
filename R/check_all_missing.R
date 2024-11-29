#' @noRd
check_all_missing <- function(vmat, par_name = "vmat") {

  par_name <- check_par_name(par_name)
  is_vector_or_matrix <- check_is_vector_or_matrix(vmat = vmat) || is.logical(vmat)

  if (!is_vector_or_matrix) {
    stop("Expected '", par_name, "' to be a numeric vector or matrix but got ",
         check_class(vmat))
  }

  if (!check_is_all_missing(vmat)) {
    n <- length(vmat[!is.na(vmat)])
    s <- check_singular_plural(n = n)
    stop("Expected all values of '", par_name, "' to be missing numeric but ",
         "got ", n, " non-missing value", s)
  }

  return(vmat)
}


#' @noRd
check_is_all_missing <- function(vmat) {
  (is.matrix(vmat) || is.vector(vmat) || is.logical(vmat) ||
     is.double(vmat)) && all(is.na(vmat))
}
