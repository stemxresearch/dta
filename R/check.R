#' @noRd
check_even <- function(num, par_name = "num") {
  par_name <- check_par_name(par_name)
  num <- check_numeric(num, par_name = par_name)
  is_even <- num %% 2 == 0
  if (!is_even) {
    stop("Expected '", par_name, "' to be an odd number but got ",
         check_stop(num))
  }
  return(num)
}


#' @noRd
check_is_even <- function(num) {
  num <- check_numeric(num)
  num %% 2 == 0
}
