#' Find duplicate rows based on specific columns
#'
#' \code{dta_duplicates()} identifies and returns duplicate rows in
#' a dataframe or tibble, based on the specified columns.
#'
#' @param dat A dataframe or tibble.
#' @param .columns A set of column names to check for duplicates. Defaults to
#'                 all columns.
#' 
#' @return A dataframe or tibble containing only the duplicate rows based on
#'         the specified column(s).
#'
#' @examples
#' # Create a data frame for demonstration
#' 
#' df <- data.frame(
#'   id = c(14, 20, 12, 32, 14, 23, 15, 12, 30, 14),
#'   name = c(
#'    "Mary", "Mark", "Faith", "David", "Mary", "Daniel", "Christine",
#'    "Johnson", "Elizabeth", "Mary"
#'   ),
#'   age = c(21, 18, 25, 17, 21, 24, 21, 19, 20, 21)
#' )
#' dta_gtable(df)
#' 
#' # return duplicated rows by all the columns in the data frame
#' 
#' result <- dta_duplicates(df)
#' dta_gtable(result)
#' 
#' # return duplicated rows by the column `id`
#' 
#' result2 <- dta_duplicates(df, .columns = id)
#' dta_gtable(result2)
#' 
#' # A second demo data frame
#' 
#' df2 <- data.frame(
#'  gender = c(
#'    "Male", "Female", "Female", "Male", "Male", "Female", "Male", "Female"
#'  ),
#'  education = c(
#'    "Masters", "Bachelor", "Bachelor", "Masters", "Doctorate", "Masters",
#'    "Bachelors", "Masters"
#'  ),
#'  age = c(25, 30, 30, 25, 40, 25, 40, 25)
#' )
#' dta_gtable(df2)
#' 
#' # return duplicated rows based to the columns 'gender' and 'education'
#' 
#' result2 <- dta_duplicates(df2, .columns = c(gender, education))
#' dta_gtable(result2)
#'
#' @export 
dta_duplicates <- function(dat, .columns = names(dat)) {
  dat <- check_dataframe_or_tibble(dat = dat, par_name = "dat")
  columns <- colnames(dplyr::select(dat, {{ .columns }}))
  dat <- dplyr::filter(
    dat,
    duplicated(dplyr::select(dat, dplyr::any_of(columns))) | 
      duplicated(dplyr::select(dat, dplyr::any_of(columns)), fromLast = TRUE)
  )
  
  tibble::as_tibble(dat)
}