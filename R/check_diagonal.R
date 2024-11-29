#' @noRd
check_diagonal <- function(mat, par_name = "mat") {
  if (!check_is_diagonal(mat = mat)) {
    par_name <- check_par_name(par_name)
    stop("Expected '", par_name, "' to be a diagonal matrix")
  }
  return(mat)
}


#' @noRd
check_is_diagonal <- function(mat) {
  nrow(mat) == ncol(mat) && all(mat[row(mat) != col(mat)] == 0)
}
