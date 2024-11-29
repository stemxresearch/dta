#' @noRd
check_all_numeric <- function(vmat, par_name = "vmat") {

  par_name <- check_par_name(par_name)
  vmat <- check_vector_or_matrix(vmat = vmat, par_name = par_name)
  is_all_numeric <- suppressWarnings(all(!is.na(as.numeric(vmat))))
  if (!is_all_numeric) {
    stop("Expected all values of '", par_name, "' to be numeric but got ",
         check_stop(vmat))
  }

  if (is_all_numeric) { # try converting to numeric
    tryCatch({
      vmat <- matrix(as.numeric(vmat), nrow = nrow(vmat), byrow = TRUE)
    }, error = function(e) {
      # Nothing
    })
  }

  return(vmat)
}


#' @noRd
check_is_all_numeric <- function(vmat) {
  is_vector_or_matrix <- check_is_vector_or_matrix(vmat = vmat)
  is_vector_or_matrix && suppressWarnings(all(!is.na(as.numeric(vmat))))
}
