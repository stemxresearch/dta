#' @noRd
check_iterable <- function(
    data, includes_character = FALSE, par_name = "data") {
  valid_types <- c("character", "vector", "matrix", "data.frame", "tibble")
  valid_types <- if (includes_character) valid_types else valid_types[-1]

  if (is.character(data)) {
    data_type <- "character"
  } else if (is.vector(data)) {
    if (length(data) == 1) {
      stop("Expected '", par_name, "' to have at least 2 elements but got 1 ",
           "element")
    }
    data_type <- "vector"
  } else if (is.matrix(data)) {
    data_type <- "matrix"
  } else if (is.data.frame(data)) {
    data_type <- "data.frame"
  } else if (inherits(data, c("tbl_df", "tbl"))) {
    data_type <- "tibble"
  } else {
    stop(
      check_vector_to_char(
        vec = valid_types, find_char = 'data', is_stop = TRUE
      )
    )
  }
  
  return(data)
}
