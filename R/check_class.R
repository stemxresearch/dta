#' @noRd
check_class <- function(data) {
  tryCatch({
    paste0('"', paste0(class(data), collapse = " "), '"')
  }, error = function(e) {
    "unknown"
  })
}
