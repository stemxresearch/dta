#' Transpose a data frame with specified column as variable names
#'
#' \code{dta_transpose()} transposes a data frame, optionally using a specified
#' column as the names for the resulting variables. The function can also
#' convert numeric strings to numeric values in the transposed data frame.
#'
#' @param dat A data frame or tibble to transpose.
#' @param .column_to_use_as_variables A column name to use as variable names
#'                                    in the transposed data frame. If left 
#'                                    empty, default column names
#'                                    \code{"V1", "V2", ...} will begenerated.
#' 
#' @return A transposed data frame with specified or generated column names.
#' 
#' @details
#' The function first checks if the input is a valid data frame or tibble.
#' If a column is specified via \code{.column_to_use_as_variables}, its values
#' are used as the column names for the transposed data frame, and the column
#' is removed from the input before transposing. If no column is specified, 
#' default column names are generated in the format \code{"V1", "V2", ...}.
#' 
#' After transposition, the \code{dta_to_numeric()} function is applied to
#' ensure any numeric strings in the transposed data frame are converted to
#' numeric values.
#'
#' @examples
#' data("data_cancer")
#' dta_gtable(data_cancer)
#' 
#' # Transpose the data frame and use column 1 as variable
#' # names in the transposed data frame.
#' 
#' df <- dta_transpose(
#'   dat = data_cancer, .column_to_use_as_variables = 1
#' )
#' dta_gtable(df)
#' 
#' # You could also specify the variable name
#' 
#' df2 <- dta_transpose(
#'   dat = data_cancer,
#'   .column_to_use_as_variables = cancer_type
#' )
#' dta_gtable(df2)
#' 
#' # Use default column names
#' 
#' data("data_cancer2") # does not have cancer types column
#' dta_gtable(data_cancer2)
#' 
#' # Transpose and use default column names "V1", "V2", ...
#' 
#' df3 <- dta_transpose(data_cancer2)
#' dta_gtable(df3)
#' 
#' @export
dta_transpose <- function(dat, .column_to_use_as_variables = NULL) {
  dat <- check_dataframe_or_tibble(dat, par_name = "dat")
  dplyr_cols <- rlang::enquo(.column_to_use_as_variables)
  is_null_or_missing <- rlang::quo_is_null(dplyr_cols) ||
    rlang::as_label(dplyr_cols) == "\"\""
  if (rlang::as_label(is_null_or_missing)) {
    columns <- paste0("V", 1:nrow(dat))
  } else {
    columns <- dplyr::pull(dat, {{ dplyr_cols }})
    dat <- dplyr::select(dat, -{{ dplyr_cols }})
  }
  dat <- as.data.frame(t(dat))
  dat <- dta_to_numeric(dat)
  names(dat) <- columns

  tibble::as_tibble(dat)
}
