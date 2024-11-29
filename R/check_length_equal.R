#' @noRd
check_length_equal <- function(
    x, y, allow_scalar = TRUE, is_numeric = NULL, xpar_name = "x",
    ypar_name = "y"
) {
      
  allow_scalar <- check_logical(allow_scalar, default = TRUE)
  x <- check_vector(
    x, is_numeric = is_numeric, allow_scalar = allow_scalar,
    par_name = xpar_name
  )
  y <- check_vector(
    y, is_numeric = is_numeric, allow_scalar = allow_scalar,
    par_name = ypar_name
  )

  m <- length(x)
  n <- length(y)
  is_length_equal <- m == n

  if (!is_length_equal) {
    stop("Expected '", xpar_name, "' and '", ypar_name, "' to have ",
         "the same number of elements but got ", m, " and ", n, " elements ",
         "respectively")
  }

  return(list(x = x, y = y))
}


#' @noRd
check_is_length_equal <- function(
    x, y, allow_scalar = FALSE, is_numeric = NULL
) {
  is_length_equal <- FALSE
  tryCatch({
    check_length_equal(
      x = x, y = y, allow_scalar = allow_scalar, is_numeric = is_numeric
    )
    is_length_equal <- TRUE
  }, error = function(e) {

  })
  
  return(is_length_equal)
}