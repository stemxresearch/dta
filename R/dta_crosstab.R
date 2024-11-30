#' Generate cross tabulations with optional percentages and totals
#'
#' @description
#' \code{dta_crosstab()} creates a cross-tabulation (contingency table) for
#' a given dataset, with options to include counts, row percentages, or column
#' percentages. Totals can also be added to rows, columns, or both. The output
#' is styled using the \code{janitor} package.
#'
#' @param dat A data frame (not a tibble) containing the variables to tabulate.
#' @param .row The variable to be used as rows in the crosstab.
#' @param .column The variable to be used as columns in the crosstab.
#' @param cells A character string indicating the type of values to display in
#'              the crosstab:
#'   \describe{
#'     \item{\code{"counts"}}{Display counts (default).}
#'     \item{\code{"row"}}{Display row percentages.}
#'     \item{\code{"col"}}{Display column percentages.}
#'   }
#' @param add_totals A character string specifying where to add totals:
#'   \describe{
#'     \item{\code{"both"}}{Add totals to both rows and columns (default).}
#'     \item{\code{"row"}}{Add totals to rows only.}
#'     \item{\code{"col"}}{Add totals to columns only.}
#'   }
#' @param name A character string to rename the first column in the output.
#'             Default is \code{"Variable"}.
#' @param add_percent_symbol Logical indicating whether or not to add a % sign
#'                           to percentages. Default is \code{TRUE}.
#' @param digits An integer specifying the number of decimal places to use for
#'               percentages. Default is 1.
#'
#' @return A data frame representing the cross-tabulation, styled using
#'         \code{janitor} functions.
#' The format of the output depends on the \code{cells} argument:
#' \describe{
#'   \item{Counts}{Displays raw counts.}
#'   \item{Percentages}{Displays percentages with counts in parentheses if \code{cells} is \code{"row"} or \code{"col"}.}
#' }
#'
#' @examples
#' data("data_sample")
#' df <- data_sample
#' 
#' # Crosstabulation of frequencies (counts)
#' 
#' result <- dta_crosstab(
#'   dat = df, .row = region, .column = age_group
#' )
#' dta_gtable(result)
#' 
#' # Calculate column percentages
#' 
#' result2 <- dta_crosstab(
#'   dat = df, 
#'   .row = region,
#'   .column = age_group,
#'   cells = "col",
#'   add_totals = "col"
#' )
#' dta_gtable(result2)
#' 
#' # Calculate row percentages
#' 
#' result3 <- dta_crosstab(
#'   dat = df,
#'   .row = region,
#'   .column = age_group,
#'   cells = "row",
#'   add_totals = "row"
#' )
#' dta_gtable(result3)
#' 
#' 
#' # Remove the percentages symbol
#' 
#' result4 <- dta_crosstab(
#'   dat = df,
#'   .row = region,
#'   .column = age_group,
#'   cells = "row",
#'   add_totals = "row",
#'   add_percent_symbol = FALSE
#' )
#' dta_gtable(result4)
#' 
#' @export
dta_crosstab <- function(
  dat, .row, .column, cells = c("counts", "row", "col"),
  add_totals = c("both", "row", "col"), name = "Variable",
  add_percent_symbol = TRUE, digits = 2
) {

  dat <- check_dataframe_or_tibble(dat = dat, par_name = "dat")
  row <- rlang::enquo(.row)
  column <- rlang::enquo(.column)
  cells <- match.arg(cells)[1]
  add_totals <- match.arg(add_totals)[1]
  add_totals <- if (add_totals == "both") c("row", "col") else add_totals
  name <- check_character(char = name, par_name = "name")
  add_percent_symbol <- check_logical(
    add_percent_symbol, default = TRUE
  )
  digits <- check_scalar_integer(num = digits, par_name = "digits")

  freq <- janitor::tabyl(
    dat, !!row, !!column, show_na = FALSE, show_missing_level = FALSE
  )
  names(freq)[1] = name
  freq <- janitor::adorn_totals(freq, where = add_totals)
  
  if (cells != "counts") {
    freq <- janitor::adorn_percentages(freq, denominator = cells)
    freq <- janitor::adorn_pct_formatting(freq, digits = abs(digits))
    freq <- janitor::adorn_ns(freq, position = "front")
    freq <- janitor::adorn_title(freq, placement = "combined")
      if (!add_percent_symbol) {
        freq <- dplyr::mutate(
          freq,
          dplyr::across(dplyr::everything(), ~ stringr::str_remove_all(., "%"))
        )
    }
  }

  tibble::as_tibble(freq)
}