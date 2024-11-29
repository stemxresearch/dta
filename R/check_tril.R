#' @noRd
check_tril <- function(mat, par_name = "mat") {

  mat <- check_matrix(mat = mat, par_name = "mat")

  is_lower_triangular <- nrow(mat) == ncol(mat) &&
    all(mat[row(mat) < col(mat)] == 0)

  if (!is_lower_triangular) {
    par_name <- check_par_name(par_name)
    stop("Expected '", par_name, "' to be a lower triangular matrix")
  }

  return(mat)
}


#' @noRd
check_is_tril <- function(mat) {
  is_tril <- FALSE
  tryCatch({
    check_tril(mat = mat)
    is_tril <- TRUE
  }, error = function(e) {
    
  })
  return(is_tril)
}
