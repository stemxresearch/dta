#' @noRd
check_all_positive <- function(
    vmat, is_strictly_positive = TRUE, par_name = "vmat"
) {

  par_name <- check_par_name(check_par_name)
  vmat <- check_vector_or_matrix(vmat, par_name = "vmat")
  is_strictly_positive = check_logical(is_strictly_positive, default = TRUE)
  n = length(vmat[vmat < ifelse(is_strictly_positive, 1e-32, 0)])
  n == 0

  if (!check_is_all_positive(vmat, is_strictly_positive)) {
    stop("Expected all values of '",par_name, "' to be ",
         ifelse(is_strictly_positive, "strictly ", ""), "positive but got ",
         n, " negative / zero value", check_singular_plural(n = n))
  }

  return(vmat)
}


#' @noRd
check_is_all_positive <- function(vmat, is_strictly_positive = TRUE) {
  is_all_positive = FALSE
  tryCatch({
    check_is_all_positive(
      vmat = vmat, is_strictly_positive = is_strictly_positive
    )
    is_all_positive = TRUE
  }, error = function(e) {

  })
  return(is_all_positive)
}
