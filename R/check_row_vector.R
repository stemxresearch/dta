#' @noRd
check_row_vector <- function(mat, par_name = "mat") {
  is_row_vector <- check_is_row_vector(mat)
  if (!is_row_vector) {
    par_name <- check_par_name(par_name)
    str <- ifelse(
      is.matrix(mat), paste0(nrow(mat), " rows"), check_class(mat)
    )
    stop("Expected '", par_name, "' to be a matrix with only one row but ",
         "got ", str)
  }

  return(mat)
}


#' @noRd
check_is_row_vector <- function(mat) {
  is.matrix(mat) && nrow(mat) == 1
}
