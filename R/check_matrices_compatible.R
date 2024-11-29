#' @noRd
check_matrices_compatible <- function(
  a, b, is_multiplication = FALSE, apar_name = "a", bpar_name = "b"
) {

  apar_name <- check_par_name(apar_name)
  bpar_name <- check_par_name(bpar_name)
  mata <- check_matrix(mat = a, par_name = apar_name)
  matb <- check_matrix(mat = b, par_name = bpar_name)
  is_multiplication <- check_logical(is_multiplication, default = FALSE)

  if (is_multiplication) {
    is_compatible <- ncol(mata) == nrow(matb)
    if (!is_compatible) {
      stop("Matrices '", apar_name, "' and '", bpar_name, "' are not ",
           " compatible for matrix multiplication but got ", ncol(mata), " x ",
           ncol(mata), " and ", nrow(matb), " x ", ncol(matb), " matrices",
           "respectively")
    }
  } else {
    is_compatible <- all(dim(mata) == dim(matb))
    if (!is_compatible) {
      stop("Matrix '", apar_name, "' and '", bpar_name, "' must have ",
           "the same dimensions but got ", nrow(mata), " by ", ncol(mata),
           " and ", nrow(matb), " by ", ncol(matb), " matrices respectively")
    }
  }

  return(list(a = mata, b = matb))
}


#' @noRd
check_is_matrices_compatible <- function(a, b, is_multiplication = FALSE) {
  is_matrices_compatible <- FALSE
  tryCatch({
    check_matrices_compatible(
      a = a, b = b, is_multiplication = is_multiplication
    )
    is_matrices_compatible <- TRUE
  }, error = function(e) {

  })

  return(is_matrices_compatible)
}
