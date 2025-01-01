#' Drop rows with missing values and issue a warning
#'
#' \code{dta_drop_with_warning()} removes rows with missing values (NA) from
#' specified columns in a data frame or tibble. If any rows are dropped,
#' a warning is issued indicating the number of rows removed.
#'
#' @param dat A data frame or tibble from which rows with missing values
#'            should be dropped.
#' @param .columns Columns to check for missing values. Supports tidy
#'                 selection syntax. Defaults to \code{NULL}, meaning
#'                 all columns are checked.
#'
#' @return A data frame or tibble with rows containing missing values in the
#'         specified columns removed.
#'
#' @examples
#' df <- data.frame(x = c(1, 2, NA, 4), y = c(NA, 5, 6, 7))
#' dta_gtable(df)
#' 
#' # Drop rows with NA values in any column and issue a warning
#' 
#' dta_drop_with_warning(df)
#'
#' # Drop rows with NA values in a specific column and issue a warning
#' 
#' dta_drop_with_warning(df, .columns = x)
#'
#' @seealso \code{\link[tidyr]{drop_na}}
#' @export
dta_drop_with_warning <- function(dat, .columns = names(dat)) {
  rows_before <- nrow(dat)
  dat <- tidyr::drop_na(data = dat, {{ .columns }})
  rows_after <- nrow(dat)
  n <- rows_before - rows_after
  if (n > 0) {
    s <- check_singular_plural(n = n)
    was_were <- check_singular_plural(
      n = n, singular = "was", plural = "were"
    )
    warning(n, " row", s, " with missing values (NA) ", was_were, " dropped")
  }
  
  tibble::as_tibble(dat)
}
