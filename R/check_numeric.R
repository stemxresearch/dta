#' @noRd
check_numeric <- function(
    num, min = -Inf, max = Inf, boundary = c("inclusive", "exclusive"),
    is_integer = NULL, allow_null = FALSE, par_name = "num") {
  
  par_name <- check_par_name(check_par_name)
  boundary <- match.arg(boundary)[1]

  if (!is.null(is_integer)) {
    is_integer <- check_logical(is_integer, default = FALSE)
  }

  allow_null <- check_logical(allow_null, default = FALSE)

  if (is.null(num) && !allow_null) {
    stop("Argument 'num' cannot be NULL when `allow_null = FALSE`. So either ",
         "provide a numerical scalar for 'num' or set `allow_null = TRUE`")
  }

  check_is_numeric <- function(num) {
    if (!check_is_scalar_numeric(num)) {
      stop("Expected '", par_name,"' to be a numeric scalar but got ",
           check_class(num))
    }
    return(num)
  }

  if (!is.null(num)) {
    num <- check_is_numeric(num = num)
    min <- check_is_numeric(num = min)
    max <- check_is_numeric(num = max)
  }
  
  is_valid_numeric <- !is.null(num) && (num >= min && num <= max)

  if (!is.null(num)) {
    if (!is.null(is_integer)) {
      numeric_label <- ifelse(
        is_integer,
        "an integer (whole number)",
        "a double (numeric value with decimals)"
      )
      if (is_integer && !check_is_scalar_integer(num) && !allow_null) {
        stop("Expected '", par_name, "' to be ", numeric_label, " but got ",
              check_class(num))
      }
    } else {
      numeric_label <- "a numeric value"
    }

    if (!is_valid_numeric) {
      stop("Expected '", par_name, "' to be ", numeric_label, " between ",
          min, " and ", max, " ", boundary, " but got ", num)
    }
  }

  return(num)
}


#' @noRd
check_is_numeric <- function(
    num, min = -Inf, max = Inf, boundary = c("inclusive", "exclusive"),
    is_integer = NULL, allow_null = FALSE
) {
  
  is_valid_numeric <- FALSE
  tryCatch({
    check_numeric(
      num = num, min = min, max = max, boundary = boundary,
      is_integer = is_integer, allow_null = allow_null
    )
    is_valid_numeric <- TRUE
  }, error = function(e) {
    
  })
  
  return(is_valid_numeric)
}