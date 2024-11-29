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
