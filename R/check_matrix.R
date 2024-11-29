#' @noRd
check_matrix <- function(
    mat, nrows = NULL, ncols = NULL, is_numeric = TRUE, is_square = FALSE,
    par_name = "mat"
) {

  par_name <- check_par_name(par_name)
  nrows <- check_nrows(n = nrows)
  ncols <- check_ncols(n = ncols)

  is_matrix <- is.matrix(mat)
  is_mat_square <- nrow(mat) == ncol(mat)

  if (!is_matrix) {
    stop("Expected '", par_name, "' to be a matrix but got ", check_class(mat))
  }

  is_matrix_valid <- TRUE
  if (is_square && !is_mat_square) {
    is_matrix_valid <- FALSE
    stop("Expected '", par_name, "' to be a square matrix but got a ",
         nrow(mat), " by ", ncol(mat), " matrix")
  }

  return(mat)
}


#' @noRd
check_is_matrix <- function(
    mat = mat, nrows = NULL, ncols = NULL, is_numeric = TRUE,
    is_square = FALSE) {

  is_matrix <- FALSE
  tryCatch({
    check_matrix(
      mat, nrows = nrows, ncols = ncols, is_numeric = is_numeric,
      is_square = is_square
    )
    is_matrix <- TRUE
  }, error = function(e) {

  })
  return(is_matrix)
}


#' @noRd
check_matrix_square <- function(mat, is_numeric = TRUE, par_name = "mat") {

  par_name <- check_par_name(par_name)
  is_matrix <- is.matrix(mat)
  is_numeric <- check_logical(is_numeric, default = TRUE)
  nrows <- nrow(mat)
  ncols <- ncol(mat)
  is_square <- nrows == ncols

  if (!is.numeric(mat)) {
    stop("Expected '", mat, "' to be numeric but got ", check_class(mat))
  }

  if (!is_matrix) {
    stop("Expected '", mat, "' to be a matrix but got ", check_class(mat))
  }

  if (!is_square) {
    stop("Expected '", mat, "' to be a a square matrix but got ",
         "a ", nrows, " by ", ncols, " matrix")
  }

  return(mat)
}


#' @noRd
check_is_matrix_square <- function(mat, is_numeric) {
  is_square_matrix <- FALSE
  tryCatch({
    check_matrix_square(mat = mat, is_numeric = is_numeric)
    is_square_matrix <- TRUE
  }, error = function(e) {

  })
  return(is_square_matrix)
}
