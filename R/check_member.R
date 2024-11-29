#' @noRd
check_member <- function(
  char, valid_items, default = NULL, par_name = "vec"
) {

  par_name <- check_par_name(par_name)

  is_character_scalar <- is.vector(char) && length(char) == 1

  if (!is_character_scalar) {
    stop("Expected 'char' to be a character scalar but got ", check_stop(char))
  }

  if (!is.vector(valid_items)) {
    stop("Expected 'valid_items' to be a vector but got ",
         check_class(valid_items))
  }

  is_member <- char %in% valid_items

  if (!is_member) {
    valid_items_char <- stringr::str_c(valid_items, collapse = ", ")
    if (is.null(default)) {
      stop("Expected '", par_name, "' to be one of: '", valid_items_char,
           "' but got ", check_stop(char))
    } else {
      is_character_scalar <- is.vector(default) && length(default) == 1
      if (!is_character_scalar) {
        stop("Expected 'default' to be a character scalar but got ",
              check_stop(default))
      }
      char <- default
    }
  }

  return(char)
}


#' @noRd
check_is_member <- function(char, valid_items, default = NULL) {
  is_member <- FALSE
  tryCatch({
    check_is_member(char = char, valid_items = valid_items, default = default)
    is_member <- TRUE
  }, error = function(e) {
    
  })
  return(is_member)
}