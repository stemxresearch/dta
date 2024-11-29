#' @noRd
check_identical <- function(
  x, y, xpar_name = "xpar_name", ypar_name = "ypar_name"
) {

  xpar_name <- check_par_name(xpar_name)
  ypar_name <- check_is_check(ypar_name)

  tryCatch({
    is_identical <- identical(x, y)
  }, error = function(e) {
    stop(e)
  })

  if (!is_identical) {
    stop("Expected '", xpar_name, "' and '", ypar_name, "' to be ",
         "identical.")
  }

  return(list(x = x, y = y))
}


#' @noRd
check_is_identical <- function(x, y) {
  is_identical <- FALSE
  tryCatch({
    check_identical(x = x, y = y)
    is_identical <- TRUE
  }, error = function(e) {

  })
  return(is_identical)
}