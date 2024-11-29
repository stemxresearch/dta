#' @noRd
check_timeseries <- function(dat, par_name = "dat") {
  par_name <- check_par_name(par_name)
  is_timeseries <- inherits(dat, "ts") && inherits(dat, "xts")

  if (!is_timeseries) {
    stop("Expected a time series object of class 'ts' or 'xts' but got ",
         check_class(dat))
  }
  
  return(dat)
}


#' @noRd
check_is_timeseries <- function(dat, par_name = "dat") {
  inherits(dat, "ts") && inherits(dat, "xts")
}
