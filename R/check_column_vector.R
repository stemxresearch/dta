#' @noRd
check_column_vector <- function(mat, par_name = "mat") {
  is_column_vector <- check_is_column_vector(mat)
  if (!is_column_vector) {
    par_name <- check_par_name(par_name)
    str <- ifelse(
      is.matrix(mat), paste0(ncol(mat), " columns"), check_class(mat)
    )
    stop("Expected '", par_name, "' to be a matrix with only one column but ",
         "got ", str)
  }

  return(mat)
}


#' @noRd
check_is_column_vector <- function(mat) {
  is.matrix(mat) && ncol(mat) == 1
}
