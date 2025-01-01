#' Convert numeric strings in a data frame or tibble to numeric numbers
#'
#' \code{dta_to_numeric()} takes a data frame and converts columns containing
#' numeric values represented as strings into numeric columns. Other columns
#' remain unchanged.
#'
#' @param dat A data frame where numeric strings in character columns should
#'            be converted to numeric.
#'
#' @return A data frame with numeric strings converted to numeric columns.
#'
#' @details
#' The function scans each column in the input data frame. For character
#' columns, it checks if all the values can be safely coerced to numeric.
#' If so, the column is converted to numeric using \code{as.numeric()}.
#' Non-character columns or character columns containing non-numeric values
#' remain unchanged.
#'
#' Columns are processed using \code{lapply()}, which ensures efficient
#' handling of each column.
#'
#' @examples
#' # A data frame with numeric character (a), characters (b) and numeric numbers (c)
#'
#' df <- data.frame(
#'   a = c("1", "2", "3"),
#'   b = c("A", "B", "C"),
#'   c = c(4, 5, 6)
#' )
#' dta_gtable(df)
#' str(df)
#'
#' # Convert numeric strings to numeric columns
#'
#' df <- dta_to_numeric(df)
#' dta_gtable(df)
#' str(df)
#'
#' @export
dta_to_numeric <- function(dat) {
  dat <- tibble::as_tibble(dat)
  dat[] <- lapply(dat, function(col) {
    # Check if all non-missing values can be converted to numeric
    non_missing <- col[!is.na(col)]
    is_numbers <- all(suppressWarnings(!is.na(as.numeric(non_missing))))
    if (is.character(col) && is_numbers) as.numeric(col) else col
  })

  tibble::as_tibble(dat)
}
