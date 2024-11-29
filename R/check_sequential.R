#' @noRd
check_sequential <- function(vec, start = 1, par_name = "vec") {

  par_name <- check_par_name(par_name)
  vec <- check_vector(vec, is_numeric = TRUE, par_name = "vec")
  start <- check_is_scalar_numeric(start)

  tryCatch({
    is_sequential <- all(
      vec == seq(from = start, by = 1, length.out = length(vec))
    )
  }, error = function(e) {
    tryCatch({
      is_sequential <- all(
        vec == seq(from = 1, by = 1, length.out = length(vec))
      )
    }, error = function(e) {
      stop(e)
    })
  })

  if (!is_sequential) {
    stop("Expected 'vec' to be a sequential vector but got ", check_stop(vec))
  }

  return(vec)
}


#' @noRd
check_is_sequential <- function(vec, start = 1) {
  is_sequential <- FALSE
  tryCatch({
    check_sequential(vec = vec, start = start)
    is_sequential <- TRUE
  }, error = function(e) {
    
  })
  return(is_sequential)
}