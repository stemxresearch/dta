#' Recode variables in a data frame based on a dictionary
#'
#' \code{dta_recode()} recodes variables in a \code{dat} using a dictionary
#' \code{dict}. The dictionary maps the original values of each variable to
#' their new values and labels.
#'
#' @param dat A data frame or tibble containing the variables to be recoded.
#' @param dict A data frame or tibble serving as the dictionary, specifying
#'             variable names, values, and labels.
#' @param sheet The name or index of the worksheet that contains the data
#'              for the dictionary.
#' @param min_categories Minimum number of categories for a variable to be 
#'                       recoded. Defaults to \code{1}.
#' @param max_categories Maximum number of categories for a variable to be
#'                       recoded. Defaults to \code{25}.
#' @param as_numeric Logical. If \code{TRUE}, the recoded variables are
#'                   returned as numeric. Defaults to \code{FALSE}.
#' @param is_force_sequential Logical indicating whether or not to force
#'                            sequential values, that is, they should start
#'                            at 1 and increase by 1.
#'
#' @return A tibble with recoded variables. If warnings are generated,
#'         they are saved to a CSV file and displayed.
#' 
#' @examples
#' library(dplyr)
#' data("data_sample")
#' glimpse(data_sample) # look at the data type column
#' 
#' data("dict_recode")
#' dta_gtable(dict_recode)
#' 
#' # The default nature of `dta_recode()` is to drop the
#' # labels if the values are not sequential or do not
#' # start at 1. To maintain these labels, set 
#' # `is_force_sequential` to `TRUE`. Note that this will
#' # reset the given values to sequential.
#' 
#' result2 <- dta_recode(
#'   dat = data_sample,
#'   dict = dict_recode,
#'   is_force_sequential = TRUE
#' )
#' glimpse(result2)
#' 
#' # Return numeric codes
#' 
#' result3 <- dta_recode(
#'   dat = data_sample,
#'   dict = dict_recode,
#'   as_numeric = TRUE
#' )
#' glimpse(result3) # look at the data type column and values
#' 
#' @seealso \code{\link{dta_recode_common_labels}}, \code{\link{dta_recode_auto}}, \code{\link[dplyr]{case_match}}, \code{\link[dplyr]{case_when}}
#' 
#' @export
dta_recode <- function(
    dat, dict, sheet = 1, min_categories = 2, max_categories = 25,
    as_numeric = FALSE, is_force_sequential = FALSE
) {

  dat <- check_dataframe_or_tibble(dat = dat, par_name = "dat")
  dict <- dta_process_dict(dict = dict, sheet = sheet, n = 4)
  names(dict) <- c("names", "values", "labels", "is_ordered")
  dict <- tidyr::fill(data = dict, "names", .direction = "down")
  variables <- unique(dplyr::pull(.data = dict, var = "names"))
  warn_vector <- character(0)

  original_labels <- sapply(dat, labelled::var_label)

  # Use of dplyr to handle recoding operations
  dat <- dplyr::mutate(dat, dplyr::across(dplyr::any_of(variables), ~ {
      variable <- dplyr::cur_column()

      # Skip variables not in the data
      if (!(variable %in% names(dat))) {
        warn_vector <- c(
          warn_vector,
          paste0("Variable '", variable, "' is not in the DataFrame")
        )
        return(.)
      }

      # Skip variables with too many categories
      if (length(unique(.)) < min_categories || length(unique(.)) > max_categories) {
        return(.)
      }

      # Subset data dict for the current variable
      dict_sub <- subset(dict, names == variable)
      values_col <- dplyr::pull(.data = dict_sub, var = "values")
      labels_col <- dplyr::pull(.data = dict_sub, var = "labels")
      ordered_col <- dplyr::pull(.data = dict_sub, var = "is_ordered")
      if (check_is_all_missing(ordered_col)) {
        is_ordered <- FALSE
      } else {
        is_ordered <- !(ordered_col[!is.na(ordered_col)][1] == 0)
      }

      # Ensure values are numeric if possible
      if (check_is_all_numeric(values_col)) {
        values_col <- as.numeric(values_col)
      } else {
        return(.)
      }

      # Check for duplicate numeric values
      if (length(unique(values_col)) != length(values_col)) {
        warn_vector <- c(
          warn_vector,
          paste0("Found duplicate numeric values for variable '", variable, "'")
        )
      }

      # Check for missing numeric values
      na_values <- values_col[!is.na(values_col)]
      if (length(na_values) == 0) {
        values_col <- 1:length(values_col)
      } else if (length(na_values) != length(values_col)) {
        stop("At least one of the numeric values of the variable '", variable,
             "' is missing")
      }

      # Check if the values are sequential
      if (check_is_sequential(values_col) || is_force_sequential) {
        vfactor <- factor(., levels = labels_col, ordered = is_ordered)
        vfactor <- if (as_numeric) as.numeric(vfactor) else vfactor
      } else {
        vfactor <- factor(
          ., levels = labels_col, labels = values_col, ordered = is_ordered
        )
        if (as_numeric && is.numeric(values_col)) {
          vfactor <- as.numeric(vfactor)
        }
      }
    }))

  dat <- labelled::set_variable_labels(dat, .labels = original_labels)

  # Show warnings if needed
  if (length(warn_vector) > 0) {
    cat("Warning messages\n")
    n <- max(nchar(c(warn_vector, "Warning messages")))
    cat(paste0(rep("-", times = n), collapse = ""))
    for (warn_v in warn_vector) {
      cat(warn_v, sep = "\n")
    }
    cat(paste0(rep("-", times = n), collapse = ""))
  }

  tibble::as_tibble(dat)
}


dta_process_dict <- function(dict, sheet = 1, n) {
    if (is.list(dict) || is.data.frame(dict) || tibble::is_tibble(dict)) {
        list_or_dataframe <- if(is.data.frame(dict)){
            c("data frame / tibble", "columns", ncol(dict))
        } else {
            c("list", "elements", length(dict))
        }
        m <- list_or_dataframe[3]
        if (m != n) {
            stop("Expected 'dict' to be a ", list_or_dataframe[1], " with ",
                 n, " ", list_or_dataframe[2], " but got ", m,
                 " ", list_or_dataframe[2])
        }
        dat <- tibble::as_tibble(dict)
    } else if (is.character(dict) && length(dict) == 1) {
        if (!file.exists(dict)) {
            stop("Expected 'dict' to be a valid file path but got ", dict)
        }
        dat <- dta_read(data_or_path = dict, sheet = sheet)
    } else {
        stop("Expected 'dict' to be a list, data frame, tibble or valid ",
             "file path but got ", check_class(dict))
    }
    return(dat)
}


dta_categories <- function(labels) {

    yesno1 <- c("No", "Yes")
    yesno2 <- c("No", "Yes", "Don't know")
    yesno3 <- c("No", "Yes", "Prefer not to say")
    yesno4 <- c("No", "Yes", "Don't know", "Prefer not to say")
    likert1 <- c("Strong disagree", "Disagree", "Neutral", "Agree", "Strongly agree")
    likert2 <- c("Strong disagree", "Disagree", "Undecided", "Agree", "Strongly agree")
    likert3 <- c("Strong dissatisfied", "Dissatisified", "Neutral", "Satisfied", "Strongly agree")
    likert4 <- c("Strong dissatisfied", "Dissatisified", "Undecided", "Satisfied", "Strongly agree")
    likert5 <- c("Very ineffective", "Ineffective", "Neutral", "Effective", "Very effective")
    likert6 <- c("Very ineffective", "Ineffective", "Undecided", "Effective", "Very effective")

    if (labels == "yesno1") {
        labels <- yesno1
    } else if (labels == "yesno2") {
        labels <- yesno2
    } else if (labels == "yesno3") {
        labels <- yesno3
    } else if (labels == "yesno4") {
        labels <- yesno4
    } else if (labels == "likert1") {
        labels <- likert1
    } else if (labels == "likert2") {
        labels <- likert2
    } else if (labels == "likert3") {
        labels <- likert3
    } else if (labels == "likert4") {
        labels <- likert4
    } else if (labels == "likert5") {
        labels <- likert5
    } else if (labels == "likert6") {
        labels <- likert6
    }
    return(labels)
}