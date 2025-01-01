dta_label_names <- function(dat, .columns, is_remove_names = FALSE) {
  dat <- check_dataframe_or_tibble(dat = dat, par_name = "dat")
  columns <- colnames(dplyr::select(dat, {{ .columns }}))
  is_remove_names <- check_logical(is_remove_names, default = FALSE)
  label_name <- unlist(labelled::var_label(dat[, columns, drop = FALSE]))
  if (is_remove_names) {
    names(label_name) <- NULL
  }
  if (is.null(label_name)) columns else label_name
}