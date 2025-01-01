#' Assign variable labels to data frame or tibble columns
#'
#' \code{dta_label()} assigns variable labels to the columns of a data frame or
#' tibble. It either takes a dictionary containing names and labels or takes
#' vectors of names and labels directly. This function ensures that labels are
#' applied correctly and handles missing or empty label names gracefully.
#'
#' @param dat A data frame or tibble to which the labels will be applied.
#' @param dict A data frame or tibble (optional) containing two columns 
#'             representing the variable names and their corresponding labels.
#'             If \code{NULL}, the labels will be generated from the
#'             \code{.names} and \code{.labels} arguments.
#' @param .names A character vector of variable names. This is required if
#'               \code{dict} is not provided.
#' @param .labels A character vector of variable labels corresponding to
#'                \code{.names}. This is required if \code{dict} is not
#'                provided.
#'
#' @return A tibble with the same structure as the input \code{dat}, but with
#' the variable labels applied to the columns.
#'
#' @details 
#' If \code{dict} is provided, it should contain at least two columns: 
#' which define the mapping between variable names and their labels.
#' If \code{dict} is not provided, the function will directly use the
#' \code{.names} and \code{.labels} arguments to assign labels to the variables.
#' 
#' If the \code{.names} vector is missing or has missing entries, the function
#' will attempt to use the column names from the \code{dat} object. If the
#' length of \code{.names} and the number of columns in \code{dat} do not match,
#' an error will be raised.
#'
#' @examples
#' # Using named vectors for labels
#' 
#' dat <- data.frame(
#'   age = c(25, 30, 35, 40),
#'   gender = c("Male", "Female", "Female", "Male"),
#'   income = c(50000, 60000, 55000, 65000)
#' )
#'
#' names <- c("age", "income")
#' labels <- c("Age in years", "Annual income")
#'
#' result <- dta_label(
#'   dat, dict = NULL, .names = names, .labels = labels
#' )
#' 
#' dta_gtable(result)
#'
#' # Using a dictionary data frame
#' 
#' data("data_bmi")
#' dta_gtable(head(data_bmi))
#' 
#' data("dict_labels")
#' dta_gtable(dict_labels)
#'
#' result2 <- dta_label(
#'   dat = data_bmi,
#'   dict = dict_labels,
#'   .names = names,
#'   .labels = labels
#' )
#'
#' # Proving only the `.labels` argument will rename all
#' # variables in \code{dat}. In such a case, the length of
#' # `.names` must be equal to the number of columns in
#' # \code{dat}.
#' 
#' labels <- c("Unique identifier", dict_labels$labels)
#' result3 <- dta_label(
#'   data_bmi, dict = NULL, .names = NULL, .labels = labels
#' )
#' 
#' @seealso
#' \code{\link[labelled]{set_variable_labels}}
#'
#' @export
dta_label <- function(dat, dict, .names, .labels) {
  
  dat <- check_dataframe_or_tibble(dat = dat, par_name = "dat")

  if (is.null(dict)) {
    names <- unlist(as.vector(.names))
    labels <- unlist(as.vector(.labels))
    if (is.null(names) || length(names) == 0 || any(is.na(names)) || any(!nzchar(names))) {
      names <- rep(NA, times = length(labels))
    }
    dict <- tibble::tibble(.names = names, .labels = labels)
  } else {
    dict <- check_dataframe_or_tibble(dat = dict)
    dict <- check_not_empty(dat = dict, par_name = "dict")
    names <- dplyr::pull(.data = dict, var = !!rlang::enquo(.names))
    labels <- dplyr::pull(.data = dict, var = !!rlang::enquo(.labels))
  }

  if (check_is_all_missing(names)) {
    is_nrow_ncol <- length(names) == ncol(dat)
    if (length(names) == ncol(dat)) {
      names <- names(dat)
    } else {
      stop("Column 'names' is missing entirely, provide a column with ",
           "'names' to proceed or provide a vector of ", ncol(dat),
           " in elements for the '.labels' argument"
      )
    }
  }

  for (index in seq_along(names)) {
    labelled::var_label(dat[[names[index]]]) <- labels[index]
  }

  tibble::as_tibble(dat)
}