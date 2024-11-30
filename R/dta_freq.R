#' Get frequency distribution of a specific column
#'
#' \code{dta_freq()} calculates the frequency distribution of a specified
#' column in a dataframe, with options for sorting, showing missing values,
#' and customizing the output.
#'
#' @param dat A dataframe.
#' @param .column The column in the dataframe for which the frequency
#'                distribution will be calculated.
#' @param name Optional. A name for the column in the output. Defaults to
#'              \code{NULL}.
#' @param is_sorted Logical. If \code{TRUE}, sorts the results. Defaults to
#'                  \code{FALSE}.
#' @param is_decreasing Logical. If \code{TRUE}, sorts in decreasing order.
#'                      Defaults to \code{TRUE}.
#' @param add_percent_symbol Logical indicating whether or not to add a % sign
#'                           to percentages. Default is \code{TRUE}.
#' @param digits Integer. Number of digits to round the percentage values
#'                   to. Defaults to \code{1}.
#' @return A dataframe with frequency counts, percentages, and total for the
#'         specified column.
#'
#' @examples
#' # Basic frequency distribution of 'region' column
#' 
#' data("data_sample")
#' tab <- dta_freq(dat = data_sample, .column = region)
#' dta_gtable(tab)
#'
#' # Frequency distribution with sorting in increasing
#' # order of frequency
#' 
#' tab2 <- dta_freq(
#'   dat = data_sample,
#'   .column = region,
#'   is_sorted = TRUE,
#'   is_decreasing = FALSE
#' )
#' dta_gtable(tab2)
#' 
#' # Frequency distribution with sorting in decreasing
#' # order of frequency
#' 
#' tab3 <- dta_freq(
#'   dat = data_sample,
#'   .column = region,
#'   is_sorted = TRUE,
#'   is_decreasing = TRUE
#' )
#' dta_gtable(tab3)
#' 
#' # Remove the percentage symbol
#' 
#' tab3 <- dta_freq(
#'   dat = data_sample,
#'   .column = region,
#'   is_sorted = TRUE,
#'   is_decreasing = TRUE,
#'   add_percent_symbol = FALSE
#' )
#' dta_gtable(tab3)
#'
#' @export
dta_freq <- function(
  dat, .column, name = NULL, is_sorted = FALSE, is_decreasing = TRUE,
  add_percent_symbol = TRUE, digits = 2
) {

  dat <- check_dataframe_or_tibble(dat = dat, par_name = "dat")
  column <- rlang::enquo(.column)
  if (is.null(name) || name == "") {
    name_col <- colnames(dplyr::select(dat, !!column))
    name_label <- labelled::var_label(dat[[name_col]])
    name <- if (is.null(name_label)) name_col else name_label
  } else {
    name <- check_character(char = name, par_name = "name")
  }
  is_sorted <- check_logical(lgl = is_sorted, default = FALSE)
  is_decreasing <- check_logical(lgl = is_decreasing, default = TRUE)
  digits <- check_scalar_integer(num = digits, par_name = "digits")
  freq <- janitor::tabyl(
    dat, !!column, show_na = FALSE, show_missing_level = FALSE
  )
  names(freq) = c(name, "Frequency", "Percent")
  if (is_sorted) {
    freq <- freq[order(freq[["Frequency"]], decreasing = is_decreasing), ]
  }
  freq <- janitor::adorn_totals(freq, where = "row")
  freq <- janitor::adorn_pct_formatting(freq, digits = digits)

  if (!add_percent_symbol) {
    freq <- dplyr::mutate(
      freq,
      dplyr::across(
        dplyr::where(
          ~ any(stringr::str_detect(., "%"))),
          ~ as.numeric(stringr::str_remove(., "%")
        )
      )
    )
  }
  
  tibble::as_tibble(freq)
}
