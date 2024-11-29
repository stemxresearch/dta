#' @noRd
check_vector <- function(
  vec, n = NULL, is_numeric = TRUE,
  inequality = c("==", ">", ">=", "<", "<="), allow_scalar = FALSE,
  par_name = "vec"
) {

  par_name <- check_par_name(par_name)
  is_vector_and_valid <- is.vector(vec)
  n <- check_numeric(
    num = n, min = 1, is_integer = TRUE, allow_null = TRUE, par_name = "n"
  )
  inequality <- match.arg(inequality)[1]
  allow_scalar <- check_logical(allow_scalar, default = FALSE)
  if (!is.null(is_numeric)) {
    if (!is_vector_and_valid) {
      is_numeric_char <- ifelse(is_numeric, "numeric", "character")
        stop("Expected '", par_name, "' to be a ", is_numeric_char,
             " vector with at least ", ifelse(allow_scalar, 2, 1), " elements")
    }
    if (is_numeric && !all(is.numeric(vec))) {
      stop("Expected '", par_name, "' to be a numeric vector but got ",
           check_class(vec))
    } else if (!is_numeric && !all(is.character(vec))) {
      stop("Expected '", par_name, "' to be a character vector but got ",
           check_class(vec))
    }
  }

  veclength <- length(vec)

  if (!is.null(n)) {
    comparison_result <- switch(
      inequality,
      "==" = veclength == n,
      ">" = veclength > n,
      ">=" = veclength >= n,
      "<" = veclength < n,
      "<=" = veclength <= n
    )

    if (!comparison_result) {
      comparison_text <- switch(
        inequality,
        "==" = "exactly",
        ">" = "more than",
        ">=" = "at least",
        "<" = "less than",
        "<=" = "at most"
      )
      s <- check_singular_plural(n = length(vec))
      stop("Expected '", par_name, "' to have ", comparison_text, " ", n,
            " element", s, " but got ", veclength, " element", s)
    }
  }

  return(vec)
}


#' @noRd
check_is_vector <- function(
    vec, n = NULL, is_numeric = TRUE,
    inequality = c("==", ">", ">=", "<", "<="), allow_scalar = FALSE
) {

  is_valid_vector <- FALSE
  tryCatch({
    check_vector(
      vec = vec, n = n, is_numeric = is_numeric, inequality = inequality,
      allow_scalar = allow_scalar, par_name = "vec"
    )
    is_valid_vector <- TRUE
  }, error = function(e) {

  })
  return(is_valid_vector)
}
