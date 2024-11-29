#' @noRd
check_prime <- function(num, par_name = "num") {
  par_name <- check_par_name(par_name)
  num <- check_numeric(
    num = num, min = 1, is_integer = TRUE, par_name = par_name
  )
  is_prime <- pracma::isprime(num)
  if (!is_prime) {
    stop("Expected '", par_name, "' to be a prime number but got ",
         check_stop(num))
  }
  return(num)
}


#' @noRd
check_is_prime <- function(num, par_name = "num") {
  par_name <- check_par_name(par_name)
  num <- check_numeric(
    num = num, min = 1, is_integer = TRUE, par_name = "num"
  )
  pracma::isprime(num)
}
