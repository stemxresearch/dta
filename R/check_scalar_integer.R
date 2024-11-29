#' @noRd
check_scalar_integer <- function(num, par_name = "num") {
  par_name <- check_par_name(par_name)
  if (!check_is_scalar_integer(num)) {
    stop("Expected 'num' to be an integer but got ", check_class(num))
  }
  return(num)
}


#' @noRd
check_is_scalar_integer <- function(num) {
  is.vector(num) && is.numeric(num) && length(num) == 1 &&
  num == as.integer(num)
}