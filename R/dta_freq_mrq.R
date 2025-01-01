#' Frequency table for multiple response questions
#'
#' \code{dta_freq_mrq()} generates a frequency table for multiple response
#' questions, calculating the counts and percentages for the specified values
#' (e.g., \code{"Yes"}) across multiple columns. It also adds a total row with
#' the summed counts and percentage responses.
#'
#' @param dat A data frame containing the data to be analyzed.
#' @param .columns A list of columns (or unquoted column names) to analyze.
#'                 Multiple columns can be passed.
#' @param value The value to count in the responses (default is \code{"Yes"}).
#' @param name The name to be used for the first column in the result
#'             Default is \code{"Options"}.
#' @param add_percent_symbol Logical indicating whether or not to add a % sign
#'                           to percentages. Default is \code{TRUE}.
#' @param digits The number of digits to round the percentage responses to
#'               (default is 2).
#'
#' @return A data frame containing the frequency counts, percentage responses,
#'         and total values.
#' The columns will be:
#'   \item{Options}{The different response options for the multiple response question}
#'   \item{Frequency}{The count of each response option}
#'   \item{Responses}{The percentage of each response option relative to the total count}
#'   \item{Cases}{The percentage of the category specified by \code{value}}
#'
#' @examples
#' data("data_sample")
#' 
#' # An example with multiple response variables labelled
#' # as Yes / No
#' 
#' result <- dta_freq_mrq(
#'   dat = data_sample,
#'   .columns = r:excel,
#'   value = "Yes",
#'   name = "Programming proficiency"
#' )
#' dta_gtable(result)
#' 
#' # Remove the percentage symbol
#' 
#' result2 <- dta_freq_mrq(
#'   dat = data_sample,
#'   .columns = r:excel,
#'   value = "Yes",
#'   name = "Programming proficiency",
#'   add_percent_symbol = FALSE
#' )
#' dta_gtable(result2)
#' 
#' # An example with TRUE / FALSE. First generate multiple
#' # response variables with TRUE / FALSE values
#' 
#' dat <- dta_mrq(
#'   dat = data_sample,
#'   .column = gadgets_owned,
#'   delimeter = ", ",
#'   prefix = "gad_"
#' )
#' dta_gtable(dat[1:10, (ncol(dat) - 6):ncol(dat)])
#' 
#' result3 <- dta_freq_mrq(
#'   dat = dat,
#'   .columns = starts_with("gad_"),
#'   value = TRUE,
#'   name = "Programming proficiency"
#' )
#' dta_gtable(result3)
#' 
#' # An example with numeric codes 0 / 1
#' 
#' dat2 <- dta_mrq(
#'   dat = data_sample,
#'   .column = gadgets_owned,
#'   delimeter = ", ",
#'   prefix = "gad_",
#'   as_numeric = TRUE
#' )
#' dta_gtable(dat2[1:10, (ncol(dat2) - 6):ncol(dat2)])
#' 
#' result4 <- dta_freq_mrq(
#'   dat = dat2,
#'   .columns = starts_with("gad_"),
#'   value = 1,
#'   name = "Programming proficiency"
#' )
#' dta_gtable(result4)
#' 
#' @export
dta_freq_mrq <- function(
  dat, .columns, value, name = "Options", add_percent_symbol = TRUE,
  digits = 2
) {

  columns <- colnames(dplyr::select(dat, {{ .columns }}))
  add_percent_symbol = check_logical(add_percent_symbol, default = TRUE)
  freq_list <- list()
  option_names <- character(0)
  for (column in columns) {
    freq <- dta_freq(dat, .column = column, digits = digits)
    freq <- freq[freq[[1]] == value, ]
    option_names <- c(option_names, names(freq[, 1]))
    freq_list[[column]] <- as.data.frame(freq[, -1])
  }
  freq <- dplyr::bind_rows(freq_list)
  if (nrow(freq) < 1) {
    stop("No values to tabulate. Please check your `.columns` and / `value` ",
         "columns and try again")
  }
  freq[[name]] <- option_names
  counts <- freq[, 1]
  percent_responses <- round(counts / sum(counts) * 100, digits)
  percent_cases <- as.numeric(stringr::str_remove_all(freq[, 2], "%"))
  percent_responses <- paste0(percent_responses, "%")
  freq[["col"]] <- percent_responses
  freq <- freq[, c(3, 1, 4, 2)]
  totals <- "Total"
  totals[2] <- sum(counts)
  totals[3] <- paste0(100, paste0(".", strrep("0", times = digits)), "%")
  totals[4] <- paste0(
    sum(as.numeric(stringr::str_remove_all(sum(percent_cases), "%"))), "%"
  )
  freq <- rbind(freq, totals)
  names(freq) <- c(name, "Frequency", "Responses", "Cases")

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