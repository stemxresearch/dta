#' @noRd
check_character <- function(
    char, n = NULL, inequality = c("==", ">", ">=", "<", "<="),
    par_name = "char") {

  par_name <- check_par_name(par_name)
  is_character <- is.character(char) && length(char) == 1
  nchars <- nchar(char)
  n <- check_numeric(n, min = 0, allow_null = TRUE)
  inequality <- match.arg(inequality)[1]

  if (!is_character) {
    stop("Expected '", par_name, "' to be a character vector of length 1 ",
         "but got ", check_class(char))
  }

  if (!is.null(n)) {
    comparison_result <- switch(
      inequality,
      "==" = nchars == n,
      ">" = nchars > n,
      ">=" = nchars >= n,
      "<" = nchars < n,
      "<=" = nchars <= n
    )

    if (!comparison_result) {
      is_character <- FALSE
      comparison_text <- switch(
        inequality,
        "==" = "exactly",
        ">" = "more than",
        ">=" = "at least",
        "<" = "less than",
        "<=" = "at most"
      )
      stop("Expected '", par_name, "' to have ", comparison_text, " ", n,
           " character", check_singular_plural(n = n), " but got ", nchars,
           " characters")
    }
  }

  return(char)
}


#' @noRd
check_is_character <- function(
    char, n = NULL, inequality = c("==", ">", ">=", "<", "<=")
) {
  is_valid_character <- FALSE
  tryCatch({
    check_character(
      char = char, n = n, inequality = inequality, par_name = "char"
    )
    is_valid_character <- TRUE
  }, error = function(e) {
    
  })

  return(is_valid_character)
}
