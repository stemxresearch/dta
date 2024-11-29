# =============================================================================
    # FUNCTIONS LIST
# =============================================================================

# check_all_missing
# check_all_numeric
# check_all_positive
# check_character
# check_class
# check_column_vector
# check_constant_vector
# check_dataframe_not_tibble
# check_dataframe_or_tibble
# check_diagonal
# check_diff_constant
# check_digits
# check_digits_from_tol
# check_empty
# check_even
# check_function
# check_identical
# check_interval
# check_interval_minmax
# check_interval_vector
# check_is_all_missing
# check_is_all_numeric
# check_is_all_positive
# check_is_character
# check_is_check
# check_is_column_vector
# check_is_constant_vector
# check_is_dataframe_not_tibble
# check_is_dataframe_or_tibble
# check_is_diagonal
# check_is_diff_constant
# check_is_empty
# check_is_even
# check_is_function
# check_is_identical
# check_is_interval
# check_is_interval_minmax
# check_is_interval_vector
# check_is_length_equal
# check_is_limits
# check_is_list
# check_is_matrices_compatible
# check_is_matrix
# check_is_matrix_square
# check_is_member
# check_is_not_empty
# check_is_numeric
# check_is_numeric_array
# check_is_odd
# check_is_ones
# check_is_prime
# check_is_row_vector
# check_is_scalar_character
# check_is_scalar_integer
# check_is_scalar_numeric
# check_is_sequential
# check_is_timeseries
# check_is_tril
# check_is_triu
# check_is_vector
# check_is_vector_or_matrix
# check_is_zeros
# check_iterable
# check_length_equal
# check_limits
# check_list
# check_logical
# check_matrices_compatible
# check_matrix
# check_matrix_constant
# check_matrix_square
# check_member
# check_namespace
# check_ncols
# check_not_empty
# check_nrows
# check_numeric
# check_numeric_array
# check_odd
# check_ones
# check_par_name
# check_prime
# check_row_vector
# check_scalar_character
# check_scalar_integer
# check_scalar_numeric
# check_sequential
# check_singular_plural
# check_stop
# check_timeseries
# check_tril
# check_triu
# check_vector
# check_vector_or_matrix
# check_vector_to_char
# check_zeros

# =============================================================================
    # FUNCTION CODES
# =============================================================================

dta_check <- function() {
  check_namespace(libname = "check")
}

#' @noRd
check_all_missing <- function(vmat, par_name = "vmat") {

  par_name <- check_par_name(par_name)
  is_vector_or_matrix <- check_is_vector_or_matrix(vmat = vmat) || is.logical(vmat)

  if (!is_vector_or_matrix) {
    stop("Expected '", par_name, "' to be a numeric vector or matrix but got ",
         check_class(vmat))
  }

  if (!check_is_all_missing(vmat)) {
    n <- length(vmat[!is.na(vmat)])
    s <- check_singular_plural(n = n)
    stop("Expected all values of '", par_name, "' to be missing numeric but ",
         "got ", n, " non-missing value", s)
  }

  return(vmat)
}


#' @noRd
check_is_all_missing <- function(vmat) {
  (is.matrix(vmat) || is.vector(vmat) || is.logical(vmat) ||
     is.double(vmat)) && all(is.na(vmat))
}

# =============================================================================

#' @noRd
check_all_numeric <- function(vmat, par_name = "vmat") {

  par_name <- check_par_name(par_name)
  vmat <- check_vector_or_matrix(vmat = vmat, par_name = par_name)
  is_all_numeric <- suppressWarnings(all(!is.na(as.numeric(vmat))))
  if (!is_all_numeric) {
    stop("Expected all values of '", par_name, "' to be numeric but got ",
         check_stop(vmat))
  }

  if (is_all_numeric) { # try converting to numeric
    tryCatch({
      vmat <- matrix(as.numeric(vmat), nrow = nrow(vmat), byrow = TRUE)
    }, error = function(e) {
      # Nothing
    })
  }

  return(vmat)
}


#' @noRd
check_is_all_numeric <- function(vmat) {
  is_vector_or_matrix <- check_is_vector_or_matrix(vmat = vmat)
  is_vector_or_matrix && suppressWarnings(all(!is.na(as.numeric(vmat))))
}

# =============================================================================

#' @noRd
check_all_positive <- function(
    vmat, is_strictly_positive = TRUE, par_name = "vmat"
) {

  par_name <- check_par_name(check_par_name)
  vmat <- check_vector_or_matrix(vmat, par_name = "vmat")
  is_strictly_positive = check_logical(is_strictly_positive, default = TRUE)
  n = length(vmat[vmat < ifelse(is_strictly_positive, 1e-32, 0)])
  n == 0

  if (!check_is_all_positive(vmat, is_strictly_positive)) {
    stop("Expected all values of '",par_name, "' to be ",
         ifelse(is_strictly_positive, "strictly ", ""), "positive but got ",
         n, " negative / zero value", check_singular_plural(n = n))
  }

  return(vmat)
}


#' @noRd
check_is_all_positive <- function(vmat, is_strictly_positive = TRUE) {
  is_all_positive = FALSE
  tryCatch({
    check_is_all_positive(
      vmat = vmat, is_strictly_positive = is_strictly_positive
    )
    is_all_positive = TRUE
  }, error = function(e) {

  })
  return(is_all_positive)
}

# =============================================================================

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

# =============================================================================

#' @noRd
check_class <- function(data) {
  tryCatch({
    paste0('"', paste0(class(data), collapse = " "), '"')
  }, error = function(e) {
    "unknown"
  })
}

# =============================================================================

#' @noRd
check_column_vector <- function(mat, par_name = "mat") {
  is_column_vector <- check_is_column_vector(mat)
  if (!is_column_vector) {
    par_name <- check_par_name(par_name)
    str <- ifelse(
      is.matrix(mat), paste0(ncol(mat), " columns"), check_class(mat)
    )
    stop("Expected '", par_name, "' to be a matrix with only one column but ",
         "got ", str)
  }

  return(mat)
}


#' @noRd
check_is_column_vector <- function(mat) {
  is.matrix(mat) && ncol(mat) == 1
}

# =============================================================================

#' @noRd
check_constant_vector <- function(vec, tol = 1e-16, par_name = "vec") {
  
  par_name <- check_par_name(par_name)
  vec <- check_vector(
    vec = vec, n = NULL, inequality = ">=", par_name = par_name
  )
  tol <- check_numeric(
    num = tol, min = 0, max = 1, boundary = "exclusive", par_name = par_name
  )

  vec_rounded <- round(vec, digits = -log10(tol))
  is_constant <- all(vec_rounded == vec_rounded[1])

  if (!is_constant) {
    stop("Expected '", par_name, "' to be a constant vector but got a vector",
         " with different values.")
  }
  return(vec)
}


#' @noRd
check_is_constant_vector <- function(vec, tol = 1e-16, par_name = "vec") {
  is_constant_vector <- FALSE
  tryCatch({
    check_constant_vector(vec = vec, tol = tol)
    is_constant_vector <- TRUE
  }, error = function(e) {
    
  })
  return(is_constant_vector)
}

# =============================================================================

#' @noRd
check_dataframe_not_tibble <- function(dat, par_name = "dat") {

  if (!check_is_dataframe_not_tibble(dat)) {
    stop("Expected '", par_name, "' to be a data frame (not tibble) but got ",
         check_class(dat))
  }

  return(dat)
}


#' @noRd
check_is_dataframe_not_tibble <- function(dat) {
  is.data.frame(dat) && !inherits(dat, "tbl_df")
}

# =============================================================================

#' @noRd
check_dataframe_or_tibble <- function(
  dat, nrows = NULL, ncols = NULL, par_name = "dat"
) {

  par_name <- check_par_name(par_name)
  is_dataframe_or_tibble_passed <- is.data.frame(dat) || tibble::is_tibble(dat)
  nrows <- check_numeric(nrows, min = 0, max = nrow(dat), allow_null = TRUE)
  ncols <- check_numeric(ncols, min = 0, max = ncol(dat), allow_null = TRUE)

  if (!is_dataframe_or_tibble_passed) {
    stop("Expected '", par_name, "' to be a DataFrame or tibble but got ",
         check_class(dat))
  }

  check_not_empty(dat = dat, par_name = par_name)

  if (!is.null(nrows)) {
    n = nrow(dat)
    if (nrows != n) {
      stop("Expected 'dat' to have ", nrows, " rows but got ", n, " rows")
    }
  }

  if (!is.null(ncols)) {
    n = ncol(dat)
    if (ncols != n) {
      stop("Expected 'dat' to have ", ncols, " columns but got ", n,
           " columns")
    }
  }

  return(dat)
}


#' @noRd
check_is_dataframe_or_tibble <- function(dat, nrows = NULL, ncols = NULL) {
  is_dataframe_or_tibble <- FALSE
  tryCatch({
    check_dataframe_or_tibble(
      dat = dat, nrows = nrows, ncols = ncols, par_name = "dat"
    )
    is_dataframe_or_tibble <- TRUE
  }, error = function(e) {

  })
  return(is_dataframe_or_tibble)
}

# =============================================================================

#' @noRd
check_diagonal <- function(mat, par_name = "mat") {
  if (!check_is_diagonal(mat = mat)) {
    par_name <- check_par_name(par_name)
    stop("Expected '", par_name, "' to be a diagonal matrix")
  }
  return(mat)
}


#' @noRd
check_is_diagonal <- function(mat) {
  nrow(mat) == ncol(mat) && all(mat[row(mat) != col(mat)] == 0)
}

# =============================================================================

#' @noRd
check_diff_constant <- function(vec, n = 2, tol = 1e-16, par_name = "vec") {
  
  par_name <- check_par_name(par_name)
  vec <- check_vector(vec, n = 2, inequality = ">=", par_name = "vec")
  n <- check_numeric(
    n, min = 1, max = length(vec), is_integer = TRUE, par_name = "n"
  )
  tol <- check_numeric(
    tol, min = 0, max = 1, boundary = "exclusive", allow_null = TRUE
  )

  digits <- check_digits_from_tol(vec = vec, tol = tol)
  vec_rounded = round(diff(vec, differences = n), digits = digits)
  is_diff_constant <- all(vec_rounded == vec_rounded[1])

  if (!is_diff_constant) {
    par_name <- check_par_name(par_name)
    stop("Difference between elements of '", par_name, "' must be constant,",
         " but got ", check_stop(vec))
  }

  return(vec)
}


#' @noRd
check_is_diff_constant <- function(vec, n = 2, tol = 1e-16) {
  is_diff_constant <- FALSE
  tryCatch({
    check_diff_constant(vec = vec, n = n, tol = tol)
    is_diff_constant <- TRUE
  }, error = function(e) {
    
  })
  return(is_diff_constant)  
}

# =============================================================================

#' @noRd
check_digits_from_tol <- function(vec, tol = 1e-16) {
  if (is.null(tol) || !is.numeric(tol) || length(tol) != 1) {
    if (tol < 0 || tol > 1) {
      tol <- 1e-16
    }
    digits <- max(
      sapply(vec, function(number) {
        if (grepl("\\.", as.character(number))) {
          nchar(strsplit(as.character(number), "\\.")[[1]][2])
        } else {
          0
        }
      })
    )
  } else {
    digits <- ceiling(-log10(tol))
  }
  return(digits)
}


#' @noRd
check_digits <- function(digits) {
  digits <- check_scalar_integer(num = digits, par_name = "digits")
  ifelse(is.null(digits), 14, digits)
}

# =============================================================================

#' @noRd
check_empty <- function(mat, par_name = "mat") {
  par_name <- check_par_name(par_name)
  mat <- check_matrix(mat = mat, par_name = "mat")
  is_empty <- nrow(mat) == 0 || ncol(mat) == 0

  if (!is_empty) {
    stop("Expected 'mat' to be empty")
  }

  return(mat)
}


#' @noRd
check_is_empty <- function(mat) {
  is_empty <- FALSE
  tryCatch({
    check_empty(mat = mat)
    is_empty <- TRUE
  }, error = function(e) {

  })
  return(is_empty)
}

# =============================================================================

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

# =============================================================================

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

# =============================================================================

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

# =============================================================================

#' @noRd
check_interval <- function(h, lower, upper) {
  h <- check_numeric(num = h, min = 1e-16, par_name = "h")
  lower <- check_numeric(num = lower, par_name = "lower")
  upper <- check_numeric(num = upper, par_name = "upper")
  
  check_limits(
    lower = min, upper = max, lower_par_name = "lower", upper_par_name = "upper"
  )

  if (h > abs(upper - lower)) {
    stop("Expected 'h' to be less than or equal to |upper - lower| = ",
         abs(upper - lower), " but got: h = ", h)
  }

  return(list(lower = lower, upper = upper, h = h))
}


#' @noRd
check_is_interval <- function(h, lower, upper) {
  is_interval <- FALSE
  tryCatch({
    check_interval(h = h, lower = lower, upper = upper)
    is_interval <- TRUE
  }, error = function(e) {

  })
  
  return(is_interval)
}

# =============================================================================

#' @noRd
check_interval_minmax <- function(num, min, max, par_name = "num") {

  par_name <- check_par_name(par_name)
  num <- check_numeric(num, par_name = par_name)
  min <- check_numeric(min, par_name = "min")
  max <- check_numeric(max, par_name = "max")

  check_limits(
    lower = min, upper = max, lower_par_name = "min", upper_par_name = "max"
  )

  is_in_interval <- num >= min && num <= max

  if (is_in_interval) {
    stop("Expected '", par_name, "' to be between ", min, " and ", max,
         " but got ", check_stop(num))
  }

  return(num)
}


#' @noRd
check_is_interval_minmax <- function(num, min, max) {
  is_interval_minmax <- FALSE
  tryCatch({
    check_interval_minmax(num = num, min = min, max = max)
    is_interval_minmax <- TRUE
  }, error = function(e) {

  })
  return(is_interval_minmax)
}


#' @noRd
check_interval_vector <- function(
    vec, min, max, action = c("warning", "stop", "none"), par_name = "vec"
) {

  par_name <- check_par_name(par_name)
  vec <- check_vector(vec, n = 2, inequality = ">=", par_name = "vec")
  min <- check_numeric(min, par_name = "min")
  max <- check_numeric(max, par_name = "max")
  check_limits(
    lower = min, upper = max, lower_par_name = "min", upper_par_name = "max"
  )
  action <- match.arg(action)[1]

  is_in_interval <- vec >= min && vec <= max

  if (!any(is_in_interval)) {
    values_outside_interval <- vec[!is_in_interval]
    s <- check_singular_plural(n = length(values_outside_interval))
    is_are <- check_singular_plural(
      n = length(values_outside_interval), singular = "is", plural = "are"
    )
    if (action == "warning") {
      warning("The value", s, " ", paste0(values_outside_interval, collapse = ", "),
              " fall outside the interval [", min, ", ", max, "]")
    } else if (action == "stop") {
      stop("Expected all values of 'vec' to be between ", min, " and ", max,
           ". The value", s, ": ", paste0(values_outside_interval, collapse = ", "),
           " fall outside the this interval")
    }
  }

  return(vec)
}


#' @noRd
check_is_interval_vector <- function(
    vec, min, max, action = c("warning", "stop", "none")
) {
  is_interval_vector <- FALSE
  tryCatch({
    check_interval_vector(vec = vec, min = min, max = max, action = action)
    is_interval_vector <- TRUE
  }, error = function(e) {

  })
  
  return(is_interval_vector)
}

# =============================================================================

#' @noRd
check_is_check <- function(is_check) {
  check_logical(is_check, default = TRUE)
}


#' @noRd
check_par_name <- function(par_name = "vec") {
 if (is.character(par_name)) par_name else "argument"
}

# =============================================================================

#' @noRd
check_iterable <- function(
    data, includes_character = FALSE, par_name = "data") {
  valid_types <- c("character", "vector", "matrix", "data.frame", "tibble")
  valid_types <- if (includes_character) valid_types else valid_types[-1]

  if (is.character(data)) {
    data_type <- "character"
  } else if (is.vector(data)) {
    if (length(data) == 1) {
      stop("Expected '", par_name, "' to have at least 2 elements but got 1 ",
           "element")
    }
    data_type <- "vector"
  } else if (is.matrix(data)) {
    data_type <- "matrix"
  } else if (is.data.frame(data)) {
    data_type <- "data.frame"
  } else if (inherits(data, c("tbl_df", "tbl"))) {
    data_type <- "tibble"
  } else {
    stop(
      check_vector_to_char(
        vec = valid_types, find_char = 'data', is_stop = TRUE
      )
    )
  }
  
  return(data)
}

# =============================================================================

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

# =============================================================================

#' @noRd
check_limits <- function(
    lower, upper, lower_par_name = "lower", upper_par_name = "upper"
) {

  lower_par_name <- check_par_name(lower_par_name)
  upper_par_name <- check_par_name(upper_par_name)
  lower <- check_numeric(lower, par_name = lower_par_name)
  upper <- check_numeric(upper, par_name = upper_par_name)

  is_valid_limits <- !is.null(lower) && !is.null(upper) && length(lower) > 0 &&
      length(upper) > 0 || lower < upper

  if (!is_valid_limits) {
    stop("Expected '", lower_par_name, "' limit to be less than '",
         upper_par_name, "' limit but got ", lower, " and ", upper,
         " respectively")
  }

  return(list(lower = lower, upper = upper))
}


#' @noRd
check_is_limits <- function(lower, upper) {
  is_limits <- FALSE
  tryCatch({
    check_limits(lower = lower, upper = upper)
    is_limits <- TRUE
  }, error = function(e) {

  })
  return(is_limits)
}

# =============================================================================

#' @noRd
check_list <- function(lst, par_name = "lst") {
  par_name <- check_par_name(par_name)
  is_list <- is.list(lst) && !check_is_dataframe_or_tibble(lst)

  if (!is_list) {
    stop("Expected '", par_name, "' to be a list but got ", check_class(lst))
  }

  return(lst)
}


#' @noRd
check_is_list <- function(lst) {
  is.list(lst) && !check_is_dataframe_or_tibble(lst)
}

# =============================================================================

#' @noRd
check_logical <- function(lgl, default) {
  ifelse(is.logical(lgl), lgl, isTRUE(default))
}

# =============================================================================

#' @noRd
check_matrices_compatible <- function(
  a, b, is_multiplication = FALSE, apar_name = "a", bpar_name = "b"
) {

  apar_name <- check_par_name(apar_name)
  bpar_name <- check_par_name(bpar_name)
  mata <- check_matrix(mat = a, par_name = apar_name)
  matb <- check_matrix(mat = b, par_name = bpar_name)
  is_multiplication <- check_logical(is_multiplication, default = FALSE)

  if (is_multiplication) {
    is_compatible <- ncol(mata) == nrow(matb)
    if (!is_compatible) {
      stop("Matrices '", apar_name, "' and '", bpar_name, "' are not ",
           " compatible for matrix multiplication but got ", ncol(mata), " x ",
           ncol(mata), " and ", nrow(matb), " x ", ncol(matb), " matrices",
           "respectively")
    }
  } else {
    is_compatible <- all(dim(mata) == dim(matb))
    if (!is_compatible) {
      stop("Matrix '", apar_name, "' and '", bpar_name, "' must have ",
           "the same dimensions but got ", nrow(mata), " by ", ncol(mata),
           " and ", nrow(matb), " by ", ncol(matb), " matrices respectively")
    }
  }

  return(list(a = mata, b = matb))
}


#' @noRd
check_is_matrices_compatible <- function(a, b, is_multiplication = FALSE) {
  is_matrices_compatible <- FALSE
  tryCatch({
    check_matrices_compatible(
      a = a, b = b, is_multiplication = is_multiplication
    )
    is_matrices_compatible <- TRUE
  }, error = function(e) {

  })

  return(is_matrices_compatible)
}

# =============================================================================

#' @noRd
check_matrix <- function(
    mat, nrows = NULL, ncols = NULL, is_numeric = TRUE, is_square = FALSE,
    par_name = "mat"
) {

  par_name <- check_par_name(par_name)
  nrows <- check_nrows(n = nrows)
  ncols <- check_ncols(n = ncols)

  is_matrix <- is.matrix(mat)
  is_mat_square <- nrow(mat) == ncol(mat)

  if (!is_matrix) {
    stop("Expected '", par_name, "' to be a matrix but got ", check_class(mat))
  }

  is_matrix_valid <- TRUE
  if (is_square && !is_mat_square) {
    is_matrix_valid <- FALSE
    stop("Expected '", par_name, "' to be a square matrix but got a ",
         nrow(mat), " by ", ncol(mat), " matrix")
  }

  return(mat)
}


#' @noRd
check_is_matrix <- function(
    mat = mat, nrows = NULL, ncols = NULL, is_numeric = TRUE,
    is_square = FALSE) {

  is_matrix <- FALSE
  tryCatch({
    check_matrix(
      mat, nrows = nrows, ncols = ncols, is_numeric = is_numeric,
      is_square = is_square
    )
    is_matrix <- TRUE
  }, error = function(e) {

  })
  return(is_matrix)
}


#' @noRd
check_matrix_square <- function(mat, is_numeric = TRUE, par_name = "mat") {

  par_name <- check_par_name(par_name)
  is_matrix <- is.matrix(mat)
  is_numeric <- check_logical(is_numeric, default = TRUE)
  nrows <- nrow(mat)
  ncols <- ncol(mat)
  is_square <- nrows == ncols

  if (!is.numeric(mat)) {
    stop("Expected '", mat, "' to be numeric but got ", check_class(mat))
  }

  if (!is_matrix) {
    stop("Expected '", mat, "' to be a matrix but got ", check_class(mat))
  }

  if (!is_square) {
    stop("Expected '", mat, "' to be a a square matrix but got ",
         "a ", nrows, " by ", ncols, " matrix")
  }

  return(mat)
}


#' @noRd
check_is_matrix_square <- function(mat, is_numeric) {
  is_square_matrix <- FALSE
  tryCatch({
    check_matrix_square(mat = mat, is_numeric = is_numeric)
    is_square_matrix <- TRUE
  }, error = function(e) {

  })
  return(is_square_matrix)
}

# =============================================================================

#' @noRd
check_matrix_constant <- function(
  mat, k = 0, is_check = TRUE, par_name = "mat"
) {

  par_name <- check_par_name(par_name)
  is_check <- check_is_check(is_check)
  mat <- check_matrix(mat, is_numeric = TRUE, par_name = par_name)
  k <- check_numeric(num = k, par_name = "num")

  if (k == 0) {
    name = "zeros"
  } else if (k == 1) {
    name <- "ones"
  } else {
     name = "constants"
  }

  is_matrix_constant <- all(mat == k, na.rm = TRUE) && !any(is.na(mat))

  if (!is_matrix_constant) {
    stop("Expected '", par_name, "' to be a ", name, " matrix.")
  }
  
  return(mat)
}


check_zeros <- function(mat, par_name = "mat") {
  check_matrix_constant(mat = mat, k = 0, is_check = TRUE, par_name = par_name)
}


#' @noRd
check_is_zeros <- function(mat) {
  check_matrix_constant(mat = mat, k = 0, is_check = FALSE, par_name = "mat")
}


#' @noRd
check_ones <- function(mat, par_name = "mat") {
  check_matrix_constant(mat = mat, k = 1, is_check = TRUE, par_name = par_name)
}


#' @noRd
check_is_ones <- function(mat) {
  check_matrix_constant(mat = mat, k = 1, is_check = FALSE, par_name = "mat")
}

# =============================================================================

#' @noRd
check_member <- function(
  char, valid_items, default = NULL, par_name = "vec"
) {

  par_name <- check_par_name(par_name)

  is_character_scalar <- is.vector(char) && length(char) == 1

  if (!is_character_scalar) {
    stop("Expected 'char' to be a character scalar but got ", check_stop(char))
  }

  if (!is.vector(valid_items)) {
    stop("Expected 'valid_items' to be a vector but got ",
         check_class(valid_items))
  }

  is_member <- char %in% valid_items

  if (!is_member) {
    valid_items_char <- stringr::str_c(valid_items, collapse = ", ")
    if (is.null(default)) {
      stop("Expected '", par_name, "' to be one of: '", valid_items_char,
           "' but got ", check_stop(char))
    } else {
      is_character_scalar <- is.vector(default) && length(default) == 1
      if (!is_character_scalar) {
        stop("Expected 'default' to be a character scalar but got ",
              check_stop(default))
      }
      char <- default
    }
  }

  return(char)
}


#' @noRd
check_is_member <- function(char, valid_items, default = NULL) {
  is_member <- FALSE
  tryCatch({
    check_is_member(char = char, valid_items = valid_items, default = default)
    is_member <- TRUE
  }, error = function(e) {
    
  })
  return(is_member)
}

# =============================================================================

#' @noRd
check_namespace <- function(libname, cat = FALSE) {

  libname <- substitute(libname) # is symbol

  if (!is.character(libname)) {
    libname <- as.character(libname)
  }

  library(libname, character.only = TRUE)

  namespace <- ls(paste0("package:", libname))

  if (cat) {
    cat(namespace, sep = "\n")
  } else {
    return(namespace)
  }

}

# =============================================================================

#' @noRd
check_ncols <- function(n, min = 1, max = Inf, allow_null = FALSE) {
  min <- check_numeric(min, par_name = "min")
  max <- check_numeric(max, par_name = "max")
  n <- check_numeric(
    num = n, min = min, max = max, is_integer = TRUE,
    allow_null = allow_null, par_name = "ncols"
  )
  return(n)
}

# =============================================================================

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

# =============================================================================

#' @noRd
check_nrows <- function(n, min = 1, max = Inf, allow_null = FALSE) {
  min <- check_numeric(min, par_name = "min")
  max <- check_numeric(max, par_name = "max")
  n <- check_numeric(
    num = n, min = min, max = max, is_integer = TRUE,
    allow_null = allow_null, par_name = "nrows"
  )

  return(n)
}

# =============================================================================

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

# =============================================================================

#' @noRd
check_numeric_array <- function(vmat, par_name = "par_name") {
  is_numeric_array <- check_is_numeric_array(vmat)
  if (!is_numeric_array) {
    stop("Expected '", vmat, "' to be a numeric array")
  }
  return(vmat)
}


#' @noRd
check_is_numeric_array <- function(vmat) {
  is.numeric(vmat) && ((is.vector(vmat) && length(vmat) >= 2) ||
    (is.array(vmat) && length(dim(vmat)) == 1 && length(vmat) >= 2) ||
    is.matrix(vmat))
}

# =============================================================================

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

# =============================================================================

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

# =============================================================================

#' @noRd
check_row_vector <- function(mat, par_name = "mat") {
  is_row_vector <- check_is_row_vector(mat)
  if (!is_row_vector) {
    par_name <- check_par_name(par_name)
    str <- ifelse(
      is.matrix(mat), paste0(nrow(mat), " rows"), check_class(mat)
    )
    stop("Expected '", par_name, "' to be a matrix with only one row but ",
         "got ", str)
  }

  return(mat)
}


#' @noRd
check_is_row_vector <- function(mat) {
  is.matrix(mat) && nrow(mat) == 1
}

# =============================================================================

#' @noRd
check_scalar_integer <- function(num, par_name = "num") {
  par_name <- check_par_name(par_name)
  if (!check_is_scalar_integer(num)) {
    stop("Expected 'num' to be an integer but got ", check_class(num))
  }
  return(num)
}


#' @noRd
check_is_scalar_integer <- function(num) {
  is.vector(num) && is.numeric(num) && length(num) == 1 &&
  num == as.integer(num)
}

# =============================================================================

#' @noRd
check_scalar_numeric <- function(num, par_name = "num") {

  if (!check_is_scalar_numeric(num)) {
    par_name <- check_par_name(par_name)
    stop("Expected '", par_name, "' to be a numeric scalar but got ",
         check_stop(num))
  }

  return(num)
}


#' @noRd
check_is_scalar_numeric <- function(num) {
  is.vector(num) && is.numeric(num) && length(num) == 1
}


#' @noRd
check_scalar_character <- function(char, par_name = "char") {

  if (!check_is_scalar_character(char)) {
    par_name <- check_par_name(par_name)
    stop("Expected '", par_name, "' to be a character scalar but got ",
         check_stop(char))
  }

  return(char)
}


#' @noRd
check_is_scalar_character <- function(char) {
  is.vector(char) && is.character(char) && length(char) == 1
}

# =============================================================================

#' @noRd
check_sequential <- function(
  vec, start = 1, is_reverse = FALSE, par_name = "vec"
) {

  par_name <- check_par_name(par_name)
  vec <- check_vector(vec, is_numeric = TRUE, par_name = "vec")
  start <- check_is_scalar_numeric(start)

  tryCatch({
    seq_args <- if (is_reverse) c(vec[length(vec)], -1) else c(1, 1)
    is_sequential <- all(
      vec == seq(from = seq_args[1], by = seq_args[2], length.out = length(vec))
    )
  }, error = function(e) {
    tryCatch({
      is_sequential <- all(
        vec == seq(from = 1, by = 1, length.out = length(vec))
      )
    }, error = function(e) {
      stop(e)
    })
  })

  if (!is_sequential) {
    stop("Expected 'vec' to be a sequential vector but got ", check_stop(vec))
  }

  return(vec)
}


#' @noRd
check_is_sequential <- function(vec, start = 1) {
  is_sequential <- FALSE
  tryCatch({
    check_sequential(vec = vec, start = start)
    is_sequential <- TRUE
  }, error = function(e) {
    
  })
  return(is_sequential)
}

# =============================================================================

#' @noRd
check_singular_plural <- function(n, singular = "", plural = "s") {
  # do not use `check...()` to avoid recursion
  stopifnot(is.numeric(n), length(n) == 1, n %% 1 == 0, n >= 0)
  stopifnot(is.character(singular), length(singular) == 1)
  stopifnot(is.character(plural), length(plural) == 1)
  ifelse(n > 1, plural, singular)
}

# =============================================================================

#' @noRd
check_stop <- function(dat) {
  if (is.matrix(dat) || is.data.frame(dat)) {
    dat <- check_class(dat)
  } else if (is.vector(dat)) {
    if (length(dat) < 10) {
      dat <- paste0(dat, collapse = ", ")
    } else {
      dat <- paste0(paste0(dat[1:min(length(dat), 5)], collapse = ", "), ", ...")
    }
  }
  return(dat)
}

# =============================================================================

#' @noRd
check_timeseries <- function(dat, par_name = "dat") {
  par_name <- check_par_name(par_name)
  is_timeseries <- inherits(dat, "ts") && inherits(dat, "xts")

  if (!is_timeseries) {
    stop("Expected a time series object of class 'ts' or 'xts' but got ",
         check_class(dat))
  }
  
  return(dat)
}


#' @noRd
check_is_timeseries <- function(dat, par_name = "dat") {
  inherits(dat, "ts") && inherits(dat, "xts")
}

# =============================================================================

#' @noRd
check_tril <- function(mat, par_name = "mat") {

  mat <- check_matrix(mat = mat, par_name = "mat")

  is_lower_triangular <- nrow(mat) == ncol(mat) &&
    all(mat[row(mat) < col(mat)] == 0)

  if (!is_lower_triangular) {
    par_name <- check_par_name(par_name)
    stop("Expected '", par_name, "' to be a lower triangular matrix")
  }

  return(mat)
}


#' @noRd
check_is_tril <- function(mat) {
  is_tril <- FALSE
  tryCatch({
    check_tril(mat = mat)
    is_tril <- TRUE
  }, error = function(e) {
    
  })
  return(is_tril)
}

# =============================================================================

#' @noRd
check_triu <- function(mat, par_name = "mat") {

  mat <- check_matrix(mat = mat, par_name = "mat")

  is_upper_triangular <- nrow(mat) == ncol(mat) &&
    all(mat[row(mat) > col(mat)] == 0)

  if (!is_upper_triangular) {
    par_name <- check_par_name(par_name)
    stop("Expected '", par_name, "' to be a upper triangular matrix.")
  }

  return(mat)
}


#' @noRd
check_is_triu <- function(mat, par_name = "mat") {
  is_triu <- FALSE
  tryCatch({
    check_triu(mat = mat)
    is_triu <- TRUE
  }, error = function(e) {
    
  })
  
  return(is_triu)
}

# =============================================================================

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

# =============================================================================

#' @noRd
check_vector_or_matrix <- function(
    vmat, allow_scalar = TRUE, par_name = "vmat") {
  
  allow_scalar <- check_logical(allow_scalar, default = TRUE)
  n <- ifelse(allow_scalar, 1, 2)
  is_vector_or_matrix <- (is.vector(vmat) && length(vmat) >= n) ||
    is.matrix(vmat)

  if (!is_vector_or_matrix) {
    par_name <- check_par_name(par_name)
    n <- ifelse(allow_scalar, 1, 2)
    s <- check_singular_plural(n = n)
    stop("Expected '", par_name, "' to be a vector with at least ", n,
         " element", s, " or a matrix but got ", check_class(vmat))
  }

  return(vmat)
}


#' @noRd
check_is_vector_or_matrix <- function(vmat, allow_scalar = TRUE) {
  is_vector_or_matrix <- FALSE
  tryCatch({
    check_vector_or_matrix(vmat = vmat, allow_scalar = allow_scalar)
    is_vector_or_matrix <- TRUE
  }, error = function(e) {
    
  })
  
  return(is_vector_or_matrix)
}

# =============================================================================

#' @noRd
check_vector_to_char <- function(
  vec, find_char, is_stop = FALSE, par_name = "vec"
) {
  vec <- check_vector(vec, is_numeric = FALSE)
  is_stop = check_logical(lgl = is_stop, default = FALSE)
  vec <- paste0(vec, collapse = "', '")
  if (is_stop) {
    find_char <- check_character(char = find_char)
    stop("Expected '", par_name, "' to be one of: '", vec, " but got ",
         find_char, "'")
  }

  return(vec)
}

# =============================================================================