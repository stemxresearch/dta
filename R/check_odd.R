#' @noRd
check_odd <- function(num, par_name = "num") {
  par_name <- check_par_name(par_name)
  num <- check_numeric(num, par_name = par_name)
  is_odd <- num %% 2 == 1
  if (!is_odd) {
    stop("Expected '", par_name, "' to be an odd number but got ",
         check_stop(num))
  }
  return(num)
}


#' @noRd
check_is_odd <- function(num) {
  num <- check_numeric(num, par_name = "num")
  num %% 2 == 1
}
