#' @noRd
check_stop <- function(dat) {
  if (is.matrix(dat) || is.data.frame(dat)) {
    dat <- check_class(dat)
  } else if (is.vector(dat)) {
    if (length(dat) < 10) {
      dat <- paste0(dat, collapse = ", ")
    } else {
      dat <- paste0(paste0(dat[1:min(length(dat), 5)], collapse = ", "), ", ...")
    }
  }
  return(dat)
}
