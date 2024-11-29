#' Automatically recode categorical variables in a data frame
#'
#' \code{dta_recode_auto()} automatically recodes categorical variables in
#' a data frame or tibble. That is, it assigns numerical codes starting
#' from 1 and increasing by 1 (i.e. 1, 2, 3, 4, ...) to the labels 
#' (categories) according to their alphabetic order.
#'
#' @param dat A data frame containing the variables to be recoded.
#' @param .columns A character vector specifying which columns to recode.
#'                Default is \code{NULL}, meaning all applicable columns will
#'                be recoded.
#' @param min_categories The minimum number of unique values to consider for
#'                       recoding to happen. Default is \code{2}.
#' @param max_categories The maximum number of unique values to consider afor
#'                       recoding to happen. Default is \code{25}.
#' @param is_ordered Logical. If \code{TRUE}, creates an ordered factor.
#'                   Default is \code{FALSE}.
#' @param as_numeric Logical. If \code{TRUE}, converts the result of recoding
#'                   to numeric, otherwise, it keeps them as factors. Default
#'                   is \code{FALSE}.
#' @details
#' The function automatically recodes categorical variables in the data frame
#' that have a number of unique values between \code{min_categories} and
#' \code{max_categories}. It assigns a numeric code to each unique category 
#' based on their alphabetical order. For instance, if a column represents
#' the variable \code{Gender} with categories \code{Female} and \code{Male},
#' the function assigns \code{1} to \code{Female} and \code{2} to \code{Male}.
#' This recoding is done using the \code{factor} function, where the levels of
#' the factor are ordered alphabetically, and then converted to numeric values.
#' This ensures that the recoding is consistent and follows a lexicographical
#' order for the categorical variables.
#'
#' @return A data frame with recoded categorical variables.
#'
#' @examples
#' library(dplyr)
#' data("data_sample")
#' glimpse(data_sample) # look at the data type column
#' 
#' # Auto-recode all categorical variables
#' 
#' result <- dta_recode_auto(dat = data_sample)
#' glimpse(result) # look at the data type column
#' 
#' # Convert to numeric
#' 
#' result <- dta_recode_auto(
#'   dat = data_sample, as_numeric = TRUE
#' )
#' glimpse(result) # look at the data type and values columns
#' 
#' # Specify the columns using the `.columns` argument
#' 
#' result2 <- dta_recode_auto(
#'   dat = data_sample, .columns = r:excel
#' )
#' glimpse(result2) # only the variables `r` through `excel` will be recoded
#'
#' @seealso \code{\link{dta_recode}}, \code{\link{dta_recode_common_labels}}, \code{\link[dplyr]{case_match}}, \code{\link[dplyr]{case_when}}
#' 
#' @export
dta_recode_auto <- function(
    dat, .columns = names(dat), min_categories = 2, max_categories = 25,
    is_ordered = FALSE, as_numeric = FALSE
) {

  dat <- check_dataframe_or_tibble(dat = dat, par_name = "dat")

  columns <- names(dplyr::select(dat, {{ .columns }}))

  min_categories = check_numeric(
    min_categories, min = 1, is_integer = TRUE, par_name = "min_categories"
  )

  max_categories = check_numeric(
    max_categories, min = min_categories, is_integer = TRUE,
    par_name = "max_categories"
  )

  if (!is.logical(is_ordered)) {
    stop("Expected 'is_ordered' to be logical")
  } else {
    if (length(is_ordered) == 1) {
      is_ordered <- check_logical(lgl = is_ordered, default = FALSE)
      is_ordered <- rep(is_ordered, times = length(columns))
    } else {
      check_length_equal(
        x = columns, y = is_ordered, xpar_name = "columns",
        ypar_name = "is_ordered"
      )
    }
  }

  as_numeric <- check_logical(as_numeric, default = FALSE)

  original_labels <- sapply(dat, labelled::var_label)

  dat <- dplyr::mutate(
    .data = dat,
    dplyr::across(
      .cols = dplyr::all_of(columns),
      .fns = ~ {
        # Identify categorical variables that need recoding
        n_unique <- length(unique(.))
        if ((is.character(.) || is.factor(.)) &&
            n_unique >= min_categories && n_unique <= max_categories) {
          if (as_numeric) {
            return(as.numeric(factor(., levels = unique(.))))
          } else {
            x <- factor(
              .,
              levels = unique(.),
              ordered = is_ordered[which(columns == dplyr::cur_column())]
            )
            return(x)
          }
        } else {
          return(.)
        }
      }
    )
  )

  dat <- labelled::set_variable_labels(dat, .labels = original_labels)

  tibble::as_tibble(dat)
}
