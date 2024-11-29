#' @noRd
check_empty <- function(mat, par_name = "mat") {
  par_name <- check_par_name(par_name)
  mat <- check_matrix(mat = mat, par_name = "mat")
  is_empty <- nrow(mat) == 0 || ncol(mat) == 0

  if (!is_empty) {
    stop("Expected 'mat' to be empty")
  }

  return(mat)
}


#' @noRd
check_is_empty <- function(mat) {
  is_empty <- FALSE
  tryCatch({
    check_empty(mat = mat)
    is_empty <- TRUE
  }, error = function(e) {

  })
  return(is_empty)
}