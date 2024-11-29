#' @noRd
check_not_empty <- function(dat, par_name = "dat") {
  msg <- "Argument 'dat' cannot be empty"
  if (inherits(dat, "data.frame") || inherits(dat, "tibble")) {
    if (nrow(dat) == 0 || ncol(dat) == 0) {
      stop(msg)
    }
  } else if (inherits(dat, "matrix")) {
    if (nrow(dat) == 0 || ncol(dat) == 0) {
      stop(msg)
    }
  } else if (is.vector(dat)) {
    if (length(dat) == 0) {
      stop(msg)
    }
  } else {
    stop("Expected 'dat' to be a DataFrame, tibble, matrix, or vector but got ",
         check_class(dat))
  }
  return(dat)
}


#' @noRd
check_is_not_empty <- function(dat) {
  is_not_empty = TRUE
  tryCatch({
    check_not_empty(dat = dat)
    is_not_empty = FALSE
  }, error = function(e) {

  })
  return(is_not_empty)
}
