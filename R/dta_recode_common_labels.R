#' Recode columns using common labels
#'
#' \code{dta_recode_common_labels()} recodes specified columns in a data frame
#' to apply consistent labels and values. It supports numeric or factor
#' conversion and handles predefined label categories.
#'
#' @param dat A data frame containing the columns to be recoded.
#' @param .columns A tidy selection of columns to recode.
#' @param labels A vector of labels to assign to the recoded values.
#'               Alternatively, a single character string
#'               (e.g., \code{"yn1"}, \code{"l1"}, etc.) for predefined label
#'               categories.
#' @param values A numeric vector of values corresponding to the labels.
#'               If `NULL`, defaults to sequential integers starting from 1.
#' @param is_reverse Logical, whether to reverse the order of values. Default
#'                   is \code{FALSE}.
#' @param is_ordered Logical, whether the recoded variables should be ordered
#'                   factors. Can be a single value or a vector corresponding
#'                   to the selected columns. Defaults to \code{NULL} (treated
#'                   as \code{FALSE} for all columns).
#' @param as_numeric Logical, whether to return the recoded columns as numeric
#'                   values rather than factors. Default is \code{FALSE}.
#' @param is_force_sequential Logical indicating whether or not to force
#'                            sequential values, that is, they should start
#'                            at 1 and increase by 1.
#' 
#' @details
#' The function allows flexible recoding by specifying custom or predefined
#' labels. If \code{labels} is a predefined category (e.g., \code{yesno1} for,
#' Yes/No) it is expanded automatically using the \code{dta_categories()}
#' function which returns the following.
#' 
#' @section Yes/No Categories:
#' \describe{
#'   \item{yesno1}{\code{c("No", "Yes")}}
#'   \item{yesno2}{\code{c("No", "Yes", "Don't know")}}
#'   \item{yesno3}{\code{c("No", "Yes", "Prefer not to say")}}
#'   \item{yesno4}{\code{c("No", "Yes", "Don't know", "Prefer not to say")}}
#' }
#'
#' @section Likert Scale Categories:
#' \describe{
#'   \item{likert1}{\code{c("Strong disagree", "Disagree", "Neutral", "Agree", "Strongly agree")}}
#'   \item{likert2}{\code{c("Strong disagree", "Disagree", "Undecided", "Agree", "Strongly agree")}}
#'   \item{likert3}{\code{c("Strong dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Strongly satisfied")}}
#'   \item{likert4}{\code{c("Strong dissatisfied", "Dissatisfied", "Undecided", "Satisfied", "Strongly satisfied")}}
#'   \item{likert5}{\code{c("Very ineffective", "Ineffective", "Neutral", "Effective", "Very effective")}}
#'   \item{likert6}{\code{c("Very ineffective", "Ineffective", "Undecided", "Effective", "Very effective")}}
#' }
#'
#' These categories can be used as quick references for consistent label
#' recoding.
#'
#' @return A modified tibble with recoded columns.
#'
#' @examples
#' library(dplyr)
#' data("data_sample")
#' dat <- data_sample[1:15, 16:21]
#'
#' # Apply the values 1 / 2 to the labels No / Yes
#' 
#' result <- dta_recode_common_labels(
#'   dat, .columns = python:spss, labels = c("No", "Yes")
#' )
#' glimpse(result) # look at data type and values columns
#'
#' # Add `as_numeric = TRUE` to return numeric
#' # values instead of factor
#' 
#' result2 <- dta_recode_common_labels(
#'   dat,
#'   .columns = python:spss,
#'   labels = c("No", "Yes"),
#'   as_numeric = TRUE
#' )
#' glimpse(result2) # look at data type and values columns
#'
#' # Use predefined label category `yesno1` which will
#' # assign 1 = No and 2 = Yes
#' 
#' result3 <- dta_recode_common_labels(
#'   dat,
#'   .columns = python:spss,
#'   labels = "yesno1",
#'   as_numeric = TRUE
#' )
#' glimpse(result3) # look at data type and values columns
#' 
#' # ======================================================
#' 
#' data("data_phone")
#' glimpse(data_phone)
#' 
#' # Create the categories
#' 
#' mrq_options <- c(
#'   "Strongly Disagree",
#'   "Disagree",
#'   "Neutral",
#'   "Agree",
#'   "Strongly Agree"
#' )
#' 
#' # Recode the columns `ease_of_use` to `design_and_appearance`
#' # as follows: 
#'   # "Strongly agree" = 1,
#'   # "Disagree" = 2,
#'   # "Neutral" = 3,
#'   # "Agree" = 4,
#'   # "Strongly agree" = 5
#' 
#' result4 <- dta_recode_common_labels(
#'   dat = data_phone,
#'   .columns = ease_of_use:design_and_appearance,
#'   labels = mrq_options,
#'   is_ordered = TRUE
#' )
#' glimpse(result4) # look at data type and values columns
#' 
#' # To reverse the codes, that is,
#'   # "Strongly agree" = 5,
#'   # "Disagree" = 4,
#'   # "Neutral" = 3,
#'   # "Agree" = 2,
#'   # "Strongly agree" = 1
#' # and return numeric values, use the following syntax
#' 
#' result5 <- dta_recode_common_labels(
#'   dat = data_phone,
#'   .columns = ease_of_use:design_and_appearance,
#'   labels = mrq_options,
#'   is_reverse = TRUE,
#'   is_ordered = TRUE,
#'   as_numeric = TRUE
#' )
#' glimpse(result5) # look at data type and values columns
#' 
#' result6 <- dta_recode_common_labels(
#'   dat = data_phone,
#'   .columns = ease_of_use:design_and_appearance,
#'   labels = mrq_options,
#'   values = LETTERS[1:5],
#'   is_ordered = TRUE,
#'   is_reverse = FALSE
#' )
#' glimpse(result6) # look at data type and values columns
#'
#' @seealso \code{\link{dta_recode}}, \code{\link{dta_recode_auto}}, \code{\link[dplyr]{case_match}}, \code{\link[dplyr]{case_when}}
#'
#' @export
dta_recode_common_labels <- function(
  dat, .columns, labels = c("No", "Yes"), values = NULL, is_reverse = FALSE,
  is_ordered = NULL, as_numeric = FALSE, is_force_sequential = FALSE
) {

  columns <- colnames(dplyr::select(dat, {{ .columns }}))
  quick_labels <- c(paste0("yesno", 1:4), paste0("likert1", 1:6))

  if (is.character(labels) && length(labels) == 1) {
    if (labels %in% quick_labels) {
      labels <- dta_categories(labels = labels)
    } else {
      stop("Expected 'labels' to be a vector or one of ",
           paste0(quick_labels, collapse = ", "), " but got ",
           check_stop(labels))
    }
  }
  ncols <- length(columns)
  ncat <- length(labels)
  columns <- rep(columns, each = ncat)
  labels <- check_vector(
    vec = labels, is_numeric = NULL, par_name = "labels"
  )
  labels <- rep(labels, times = ncols)

  if (is.null(values)) {
    values <- seq_len(ncat)
  } else {
    values <- check_vector(
      vec = values, is_numeric = NULL, par_name = "values"
    )
  }

  values <- rep(values, times = ncols)
  check_length_equal(
    x = labels, y = values, xpar_name = "x", ypar_name = "y"
  )

  if (!is.null(is_ordered)) {
    is_ordered <- check_logical(is_ordered, default = FALSE)
    is_ordered_temp <- rep(is_ordered, times = length(columns))
    if (length(is_ordered) == 1) {
      is_ordered <- is_ordered_temp
    } else {
      if (length(is_ordered) == ncols) {
        is_ordered <- rep(is_ordered, each = ncols)
      } else {
        stop("Expected 'is_ordered' to be a logical vector with 1 or ", ncols,
             " elements but got ", length(is_ordered), " elements")
      }
    }
  } else {
    is_ordered <- rep(FALSE, times = length(columns))
  }

  as_numeric <- check_logical(as_numeric)
  dict <- tibble::tibble(names = columns, values, labels, is_ordered)
  
  if (as_numeric || is.character(values)) {
    dat <- dta_recode_map(
      dat = dat, .columns = columns, labels = labels, values = values,
      is_reverse = is_reverse
    )
  } else {
    dat <- dta_recode(
      dat = dat, dict = dict, as_numeric = FALSE,
      is_force_sequential = is_force_sequential
    )
  }

  tibble::as_tibble(dat)
}


dta_recode_map <- function(
  dat, .columns = names(dat), labels, values = NULL, is_reverse = FALSE
) {

  dat <- tibble::as_tibble(dat)
  columns <- colnames(dplyr::select(dat, {{ .columns }}))
  labels <- unlist(as.vector(labels))
  if (is.null(values)) {
    values <- seq_len(length(labels))
  } else {
    values <- unlist(as.vector(values))
  }
  check_length_equal(x = labels, y = values)
  is_reverse <- check_logical(is_reverse, default = FALSE)
  values <- if (is_reverse) rev(values) else values
  mapping <- stats::setNames(values, labels)
  dat <- dplyr::mutate(
    dat,
    dplyr::across(dplyr::all_of(columns), ~ mapping[match(.x, names(mapping))])
  )
  
  tibble::as_tibble(dat)
}