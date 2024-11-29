#' @noRd
check_triu <- function(mat, par_name = "mat") {

  mat <- check_matrix(mat = mat, par_name = "mat")

  is_upper_triangular <- nrow(mat) == ncol(mat) &&
    all(mat[row(mat) > col(mat)] == 0)

  if (!is_upper_triangular) {
    par_name <- check_par_name(par_name)
    stop("Expected '", par_name, "' to be a upper triangular matrix.")
  }

  return(mat)
}


#' @noRd
check_is_triu <- function(mat, par_name = "mat") {
  is_triu <- FALSE
  tryCatch({
    check_triu(mat = mat)
    is_triu <- TRUE
  }, error = function(e) {
    
  })
  
  return(is_triu)
}
