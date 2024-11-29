#' @noRd
check_function <- function(fexpr, nvars = NULL, par_name = "fexpr") {

  par_name <- check_par_name(par_name)
  is_function <- is.function(fexpr)
  nvars <- check_numeric(nvars, min = 1, allow_null = TRUE, par_name = "nvars")
  if (!is_function) {
    stop("Expected '", par_name, "' to be an R function specifying ",
         "a mathematical equation but got ", check_stop(fexpr))
  }

  # Get the names of the formal arguments of "f"
  vars <- names(formals(fexpr))
  n <- length(vars)

  if (!is.null(nvars)) {
    s <- check_singular_plural(n = nvars)
    if (n != nvars) {
      stop("Expected 'f' to have ", nvars, " variable", s, " but got ",
          check_stop(vars))
    }
  }

  return(fexpr)
}


#' @noRd
check_is_function <- function(fexpr, nvars = NULL) {
  is_function <- FALSE
  tryCatch({
    check_function(fexpr = fexpr, nvars = nvars)
    is_function <- TRUE
  }, error = function(e) {

  })

  return(is_function)
}