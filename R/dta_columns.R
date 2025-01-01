#' Retrieve column names from a data frame
#'
#' \code{dta_columns()} retrieves the names of specified columns from
#' a data frame or tibble.
#'
#' @param dat A data frame or tibble from which to select columns.
#' @param .columns Column name(s) to be selected from \code{dat}. Supports
#'                 tidy selection syntax.
#' @param is_one_column A logical value indicating whether only one column
#'                      should be selected. If \code{TRUE} and more than one
#'                      column is selected, an error is thrown. Default is
#'                      \code{FALSE}.
#'
#' @return A character vector containing the names of the selected columns.
#'
#' @examples
#' data(mtcars)
#' dta_gtable(head(mtcars))
#' 
#' dta_columns(mtcars, .columns = starts_with("c"))
#' 
#' dta_columns(mtcars, .columns = cyl:wt)
#' 
#' dta_columns(mtcars, .columns = c(mpg, hp, vs, gear))
#' 
#' try(dta_columns(mtcars, .columns = c(mpg, hp, vs, gear), is_one_column = TRUE))
#' 
#' @export
dta_columns <- function(dat, .columns, is_one_column = FALSE) {

  dat <- check_dataframe_or_tibble(dat, par_name = "dat")
  is_one_column <- check_logical(is_one_column, default = FALSE)
  columns <- colnames(dplyr::select(dat, {{ .columns }}))

  if (is_one_column && length(columns) != 1) {
    stop("Expected 1 column but got ", length(columns), " columns")
  }

  return(columns)
}
