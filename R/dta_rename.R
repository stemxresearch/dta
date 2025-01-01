#' Rename columns in a data frame and tibble based on a dictionary
#'
#' \code{dta_rename()} renames columns in a data frame \code{dat} using
#' a dictionary \code{dict}. The dictionary specifies the old column names and
#' their corresponding new names.
#'
#' @param dat A data frame whose columns are to be renamed.
#' @param dict A data frame serving as the dictionary, containing the old and
#'             new variable names.
#' @param .oldnames A column in \code{dict} specifying the old variable names.
#' @param .newnames A column in \code{dict} specifying the new variable names.
#'
#' @details
#' The function checks the following:
#' \itemize{
#'   \item Both \code{dat} and \code{dict} are data frames or tibbles.
#'   \item Columns specified in \code{.oldnames} exist in \code{dict}.
#'   \item The new variable names \code{.newnames} are unique.
#'   \item There is no overlap between \code{.oldnames} and \code{.newnames} to avoid conflicts.
#' }
#'
#' If any issues are detected, an error or warning is raised to ensure proper
#' processing.
#' 
#' @return A tibble with renamed columns.
#'
#' @examples
#' data("data_bmi")
#' dta_gtable(data_bmi)
#'
#' # Rename the columns `age` and `weight` to `age_in_years`
#' # and `weight_in_kgs`
#'
#' result <- dta_rename(
#'   dat = data_bmi,
#'   .oldnames = c("age", "weight"),
#'   .newnames = c("age_in_years", "weight_in_kgs")
#')
#'
#' dta_gtable(result)
#'
#' # Rename all columns to `unique_id`, `age_in_years`,
#' # `height_in_meters` and `weight_in_kgs`. In this case,
#  # you do not have to specify old column names - just
#  # set `.oldnames` to NA
#' 
#' new_names <- c(
#'   "unique_id",
#'   "age_in_years",
#'   "height_in_meters",
#'   "weight_in_kgs"
#' )
#' result2 <- dta_rename(
#'   dat = data_bmi,
#'   .oldnames = NA,
#'   .newnames = new_names
#')
#'
#' dta_gtable(result2)
#'
#' # Rename from a dictionary - Begin by loading the dataset
#' # with columns to be renamed.
#' 
#' data("data_rename")
#' dta_gtable(head(data_rename))
#' 
#' # The dictionary with the columns `oldnames` and
#' # `newnames` representing the old and new variable names
#' # respectively
#' 
#' data("dict_rename")
#' dta_gtable(dict_rename)
#' 
#' # Perform the renaming
#' 
#' result3 <- dta_rename(
#'   dat = data_rename,
#'   dict = dict_rename,
#'   .oldnames = old,
#'   .newnames = new
#')
#'
#' dta_gtable(result3)
#'
#' @export
dta_rename <- function(dat, dict = NULL, .oldnames, .newnames) {

  dat <- check_dataframe_or_tibble(dat = dat, par_name = "dat")

  if (is.null(dict)) {
    oldnames <- unlist(as.vector(.oldnames))
    newnames <- unlist(as.vector(.newnames))
    if (is.null(oldnames) || length(oldnames) == 0 || any(is.na(oldnames)) || any(!nzchar(oldnames))) {
      oldnames <- rep(NA, times = length(newnames))
    }
    dict <- tibble::tibble(.oldnames = oldnames, .newnames = newnames)
    dup_varnames <- dta_duplicates(dat = dict, .columns = names(dict)[2])
  } else {
    dict <- check_dataframe_or_tibble(dat = dict)
    dict <- dta_drop_with_warning(dict, .columns = !!rlang::enquo(.newnames))
    dict <- check_not_empty(dat = dict, par_name = "dict")
    oldnames <- dplyr::pull(.data = dict, var = !!rlang::enquo(.oldnames))
    newnames <- dplyr::pull(.data = dict, var = !!rlang::enquo(.newnames))
    col_name <- colnames(dplyr::select(dict, !!rlang::enquo(.newnames)))
    dup_varnames <- dta_duplicates(dat = dict, .columns = col_name)
  }

  if (check_is_all_missing(oldnames)) {
    is_nrow_ncol <- length(oldnames) == ncol(dat)
    if (length(oldnames) == ncol(dat)) {
      oldnames <- names(dat)
    } else {
      stop("'.oldnames' is missing entirely, provide a column with ",
           "'.oldnames' to proceed or provide a vector of ", ncol(dat),
           " in '.newnames' arguments"
      )
    }
  }

  if (nrow(dup_varnames) > 0) {
    stop("Expected '.newnames' to be unique but got duplicates: ",
         check_stop(unique(dup_varnames[[2]])))
  }

  vars_in_both <- dplyr::intersect(oldnames, newnames)
  if (length(vars_in_both) > 0) {
    stop("Expected '.oldnames' and '.newnames' NOT to have common variable ",
         "names but got '", check_stop(vars_in_both),
         "' in both columns")
  }

  oldnew_names <- stats::setNames(object = oldnames, nm = newnames)
  dat <- dplyr::rename(dat, dplyr::all_of(oldnew_names))

  tibble::as_tibble(dat)
}
