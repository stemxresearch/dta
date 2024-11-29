#' @noRd
check_scalar_numeric <- function(num, par_name = "num") {

  if (!check_is_scalar_numeric(num)) {
    par_name <- check_par_name(par_name)
    stop("Expected '", par_name, "' to be a numeric scalar but got ",
         check_stop(num))
  }

  return(num)
}


#' @noRd
check_is_scalar_numeric <- function(num) {
  is.vector(num) && is.numeric(num) && length(num) == 1
}


#' @noRd
check_scalar_character <- function(char, par_name = "char") {

  if (!check_is_scalar_character(char)) {
    par_name <- check_par_name(par_name)
    stop("Expected '", par_name, "' to be a character scalar but got ",
         check_stop(char))
  }

  return(char)
}


#' @noRd
check_is_scalar_character <- function(char) {
  is.vector(char) && is.character(char) && length(char) == 1
}
