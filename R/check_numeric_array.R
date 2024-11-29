#' @noRd
check_numeric_array <- function(vmat, par_name = "par_name") {
  is_numeric_array <- check_is_numeric_array(vmat)
  if (!is_numeric_array) {
    stop("Expected '", vmat, "' to be a numeric array")
  }
  return(vmat)
}


#' @noRd
check_is_numeric_array <- function(vmat) {
  is.numeric(vmat) && ((is.vector(vmat) && length(vmat) >= 2) ||
    (is.array(vmat) && length(dim(vmat)) == 1 && length(vmat) >= 2) ||
    is.matrix(vmat))
}