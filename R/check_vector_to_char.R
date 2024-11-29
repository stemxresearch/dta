#' @noRd
check_vector_to_char <- function(
  vec, find_char, is_stop = FALSE, par_name = "vec"
) {
  vec <- check_vector(vec, is_numeric = FALSE)
  is_stop = check_logical(lgl = is_stop, default = FALSE)
  vec <- paste0(vec, collapse = "', '")
  if (is_stop) {
    find_char <- check_character(char = find_char)
    stop("Expected '", par_name, "' to be one of: '", vec, " but got ",
         find_char, "'")
  }

  return(vec)
}
