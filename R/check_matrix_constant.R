#' @noRd
check_matrix_constant <- function(
  mat, k = 0, is_check = TRUE, par_name = "mat"
) {

  par_name <- check_par_name(par_name)
  is_check <- check_is_check(is_check)
  mat <- check_matrix(mat, is_numeric = TRUE, par_name = par_name)
  k <- check_numeric(num = k, par_name = "num")

  if (k == 0) {
    name = "zeros"
  } else if (k == 1) {
    name <- "ones"
  } else {
     name = "constants"
  }

  is_matrix_constant <- all(mat == k, na.rm = TRUE) && !any(is.na(mat))

  if (!is_matrix_constant) {
    stop("Expected '", par_name, "' to be a ", name, " matrix.")
  }
  
  return(mat)
}


check_zeros <- function(mat, par_name = "mat") {
  check_matrix_constant(mat = mat, k = 0, is_check = TRUE, par_name = par_name)
}


#' @noRd
check_is_zeros <- function(mat) {
  check_matrix_constant(mat = mat, k = 0, is_check = FALSE, par_name = "mat")
}


#' @noRd
check_ones <- function(mat, par_name = "mat") {
  check_matrix_constant(mat = mat, k = 1, is_check = TRUE, par_name = par_name)
}


#' @noRd
check_is_ones <- function(mat) {
  check_matrix_constant(mat = mat, k = 1, is_check = FALSE, par_name = "mat")
}
