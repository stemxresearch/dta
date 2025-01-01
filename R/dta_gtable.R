#' Create a styled gt table
#'
#' \code{dta_gtable()} creates a \code{gt} table from the input data frame and
#' applies a consistent styling with vertical borders, bold column labels,
#' reduced padding, and customized background colors.
#'
#' @param dat A data frame that you want to render and style as a \code{gt}
#'            table.
#'
#' @return A styled \code{gt} table object.
#'
#' @examples
#' data("women")
#' dta_gtable(women)
#'
#' data("mtcars")
#' dta_gtable(head(mtcars, n = 10))
#'
#' @export
dta_gtable <- function(dat) {

  tryCatch({
    dat <- tibble::as_tibble(dat)
    dat <- tidyr::replace_na(dat, list_fill = "-")
  }, error = function(e) {
    stop("Unable to convert 'data' to a tibble. ", e)
  })

  # Create the initial gt table
  gtable <- gt::gt(dat)

  # Replace NA with empty string within gt syntax
  gtable <- gt::sub_missing(
    gtable,
    columns = dplyr::everything(),
    missing_text = ""
  )

  # Apply cell border styles to the body
  gtable <- gt::tab_style(
    gtable,
    style = gt::cell_borders(
      sides = c("left", "right"), color = "#ddd", weight = gt::px(1)
    ),
    locations = gt::cells_body(columns = dplyr::everything())
  )

  # Apply cell border styles to the column labels
  gtable <- gt::tab_style(
    gtable,
    style = gt::cell_borders(
      sides = c("left", "right"), color = "#ccc", weight = gt::px(1)
    ),
    locations = gt::cells_column_labels(columns = dplyr::everything())
  )

  # Apply table options
  gtable <- gt::tab_options(
    gtable,
    column_labels.font.weight = "bold",
    data_row.padding = gt::px(4),
    column_labels.padding = gt::px(4),
    heading.padding = gt::px(4),
    column_labels.background.color = "#EEE",
    table.align = "left"
  )

  # Apply border styles to all sides of the body
  gtable <- gt::tab_style(
    gtable,
    style = gt::cell_borders(
      sides = "all", color = "#ccc", weight = gt::px(0.5), style = "solid"
    ),
    locations = gt::cells_body()
  )

  # Left-align character columns
  character_columns <- names(dat)[sapply(dat, is.factor)]
  if (length(character_columns) > 0) {
    gtable <- gt::cols_align(
      gtable,
      align = "right",
      columns = character_columns
    )
  }

  return(gtable)
}
