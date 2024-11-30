#' Correct misspelled data using a dictionary
#'
#' \code{dta_replace()} corrects misspelled entries in a data frame or tibble
#' based on a provided dictionary. The dictionary specifies the correct values
#' for misspelled entries in a specified column.
#'
#' @param dat A data frame or tibble containing the data to be corrected.
#' @param dict A data frame or tibble serving as the dictionary, with columns
#'             specifying the correct and incorrect spellings.
#' @param .name The column in both \code{dat} and \code{dict} to match entries
#'              by (e.g., a unique identifier).
#' @param .wrong The column in \code{dict} containing the misspelled values to
#'               be corrected.
#' @param .correct The column in \code{dict} containing the correct values for
#'                 the misspelled entries.
#'
#' @return A data frame or tibble with corrected entries.
#' @details 
#' The function first validates that \code{dat} and \code{dict} are data frames
#' or tibbles. It then fills missing values in the \code{dict} for the columns
#' specified in \code{.name} and \code{.correct}, using a downward fill
#' strategy. Finally, it replaces misspelled values in \code{dat} using a
#' dictionary lookup facilitated by \code{matchmaker::match_df()}.
#'
#' @examples
#' # Example data with misspelled characters / strings
#' 
#' data("data_misspelled")
#' dta_gtable(head(data_misspelled))
#' 
#' data("dict_misspelled")
#' dta_gtable(dict_misspelled)
#'
#' # Correct the misspelled entries in `dat` using the
#' # `dict` dictionary
#' 
#' result <- dta_replace(
#'   dat = data_misspelled, 
#'   dict = dict_misspelled, 
#'   .name = variable, 
#'   .wrong = old, 
#'   .correct = new
#' )
#' dta_gtable(head(result))
#'
#' @export
dta_replace <- function(dat, dict, .name, .wrong, .correct) {
  
  dat <- check_dataframe_or_tibble(dat = dat, par_name = "dat")
  dict <- check_dataframe_or_tibble(dat = dict, par_name = "dict")
  dict <- tidyr::fill(
    data = dict, c({{ .name }}, {{ .correct }}), .direction = "down"
  )
  columns <- colnames(
    dplyr::select(dict, c({{ .name }}, {{ .wrong }}, {{ .correct }}))
  )
  matchmaker::match_df(
    x = dat, dictionary = dict, from = columns[2], to = columns[3],
    by = columns[1]
  )
}
