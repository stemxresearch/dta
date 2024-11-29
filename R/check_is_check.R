#' @noRd
check_is_check <- function(is_check) {
  check_logical(is_check, default = TRUE)
}


#' @noRd
check_par_name <- function(par_name = "vec") {
 if (is.character(par_name)) par_name else "argument"
}
