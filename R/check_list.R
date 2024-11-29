#' @noRd
check_list <- function(lst, par_name = "lst") {
  par_name <- check_par_name(par_name)
  is_list <- is.list(lst) && !check_is_dataframe_or_tibble(lst)

  if (!is_list) {
    stop("Expected '", par_name, "' to be a list but got ", check_class(lst))
  }

  return(lst)
}


#' @noRd
check_is_list <- function(lst) {
  is.list(lst) && !check_is_dataframe_or_tibble(lst)
}
