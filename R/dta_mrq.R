#' Split multiple response question column into binary columns
#'
#' \code{dta_mrq()} splits a specified column in a data frame that contains
#' multiple responses into multiple binary columns. Each binary column
#' represents whether a given response option exists in the original data.
#' It also allows for custom labeling of the new columns, with options for
#' numeric conversion and clean column names.
#'
#' @param dat A data frame containing the column to be split.
#' @param .column The name of the column to split.
#' @param delimeter A string representing the delimiter used to separate the
#'                  different responses in the original column.
#' @param prefix A string that is added as a prefix to the newly created column
#'               names. Default is \code{NULL}, which means no prefix will be
#'               added.
#' @param as_numeric Logical. If \code{TRUE}, the new columns will be converted
#'                   to numeric (\code{1} for \code{TRUE}, \code{0} for
#'                   \code{FALSE}). Default is \code{FALSE}.
#' @param labels A vector of length \code{2} specifying the labels for the
#'               \code{TRUE} and \code{FALSE} values in the binary columns.
#'               Default is \code{c(TRUE, FALSE)}.
#' @param is_clean_names Logical. If \code{TRUE}, \code{janitor::clean_names()}
#'                       is used to standardize the column names (e.g., convert
#'                       to lowercase and replace spaces with underscores).
#'                       Default is \code{TRUE}.
#'
#' @return A data frame with new binary columns added. The new columns
#'         represent each of the unique responses found in the original column,
#'         with the option to clean column names.
#'
#' @details
#' This function is typically used when dealing with survey data where multiple
#' options may be selected for a given question, and you want to split those
#' options into individual binary columns indicating the presence or absence
#' of each option.
#'
#' @examples
#' data("data_gadgets")
#' dat <- data_gadgets
#' dta_gtable(dat)
#' 
#' # Split `gadgets_owned` column into separate columns.
#' # The created columns will be logical (i.e. TRUE / FALSE).
#' 
#' df <- dta_mrq(
#'  dat = dat,
#'  .column = gadgets_owned,
#'  delimeter = ", ",
#'  is_clean_names = TRUE)
#' dta_gtable(df)
#' 
#' # Convert the created columns from logical (TRUE / FALSE)
#' # columns to numeric.
#' 
#' df2 <- dta_mrq(
#'  dat = dat,
#'  .column = gadgets_owned,
#'  delimeter = ", ",
#'  as_numeric = TRUE,
#'  is_clean_names = TRUE
#')
#' dta_gtable(df2)
#' 
#' # You can specify the labels to be used. In the example
#' # below, the columns will be character with Yes / No.
#' 
#' df3 <- dta_mrq(
#'  dat = dat,
#'  .column = gadgets_owned,
#'  delimeter = ", ",
#'  labels = c("Yes", "No"),
#'  is_clean_names = TRUE,
#')
#' dta_gtable(df3)
#' 
#' # Any other labels could be used. For example
#' # Positive / Negative e.g. in the case of diseases.
#' 
#' df4 <- dta_mrq(
#'  dat = dat,
#'  .column = gadgets_owned,
#'  delimeter = ", ",
#'  labels = c("Positive", "Negative"),
#'  is_clean_names = TRUE,
#')
#' dta_gtable(df4)
#' 
#' # Use numeric values and specify a `prefix` for the
#' # column names.
#' 
#' df5 <- dta_mrq(
#'  dat = dat,
#'  .column = gadgets_owned,
#'  delimeter = ", ",
#'  prefix = "gad_",
#'  labels = c(1, 2),
#'  is_clean_names = TRUE
#')
#' dta_gtable(df5)
#' 
#' @export
dta_mrq <- function(
  dat, .column, delimeter, prefix = NULL, as_numeric = FALSE,
  labels = c(TRUE, FALSE), is_clean_names = TRUE
) {

  column <- rlang::enquo(.column)
  delimeter <- check_character(char = delimeter, par_name = "char")
  prefix <- if (!is.null(prefix)) {
    check_character(char = prefix, par_name = "prefix")
  } else {
    ""
  }
  as_numeric <- check_logical(lgl = as_numeric, default = FALSE)
  labels <- check_vector(
    vec = labels, is_numeric = NULL, n = 2, inequality = "==",
    par_name = "labels"
  )
  is_clean_names <- check_logical(lgl = is_clean_names, default = TRUE)

  varname <- colnames(dplyr::select(dat, !!column))
  split_column <- stringr::str_split(
    dplyr::pull(dat, !!column), pattern = delimeter
  )
  options_unique <- unique(stringr::str_trim(unlist(split_column)))
  options_unique <- options_unique[nzchar(options_unique)]

  for (option in options_unique) {
    col_name <- paste0(prefix, option)
    dat[[col_name]] <- purrr::map_lgl(split_column, ~ option %in% .x)
    labelled::var_label(dat[[col_name]]) <- option
  }
  
  original_labels <- sapply(dat, labelled::var_label)
  labels <- if (as_numeric) c(1, 0) else labels
  if (!is.null(prefix)) {
    options_unique <- paste0(prefix, options_unique)
  }
  dat <- dplyr::mutate(
    dat, dplyr::across(
      dplyr::any_of(options_unique), ~ ifelse(. == TRUE, labels[1], labels[2])
    )
  )
  dat <- labelled::set_variable_labels(dat, .labels = original_labels)

  dat <- if (is_clean_names) janitor::clean_names(dat) else dat
  tibble::as_tibble(dat)
}