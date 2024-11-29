#' @noRd
check_dataframe_not_tibble <- function(dat, par_name = "dat") {

  if (!check_is_dataframe_not_tibble(dat)) {
    stop("Expected '", par_name, "' to be a data frame (not tibble) but got ",
         check_class(dat))
  }

  return(dat)
}


#' @noRd
check_is_dataframe_not_tibble <- function(dat) {
  is.data.frame(dat) && !inherits(dat, "tbl_df")
}
