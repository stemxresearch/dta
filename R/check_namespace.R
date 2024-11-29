#' @noRd
check_namespace <- function(libname, cat = FALSE) {

  libname <- substitute(libname) # is symbol

  if (!is.character(libname)) {
    libname <- as.character(libname)
  }

  library(libname, character.only = TRUE)

  namespace <- ls(paste0("package:", libname))

  if (cat) {
    cat(namespace, sep = "\n")
  } else {
    return(namespace)
  }

}
