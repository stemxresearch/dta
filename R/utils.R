#' Export the sample datasets to file
#'
#' \code{dta_get_datasets()} copies a dataset from the \code{code} installation
#' to a specified file path. If no path is provided, it saves the file as
#' \code{dta-data.xlsx} in the current working directory.
#'
#' @param copy_to_file_path A string specifying the file path where the
#'                          dataset should be copied. If \code{NULL} (default),
#'                          the dataset will be copied to \code{dta-data.xlsx}
#'                          in the current working directory.
#'
#' @return The function does not return a value. It prints a success message
#' if the operation is successful or throws an error if it fails.
#'
#' @details 
#' The function uses \code{file.copy()} to copy the file specified by
#' \code{dta_path()} to the destination path. The destination path can be
#' provided as an argument, or it defaults to \code{dta-data.xlsx} in the
#' current working directory.
#'
#' @examples
#' # Copy to the default location
#' # dta_get_datasets()
#'
#' @seealso \code{\link{dta_path}} \code{\link[base]{file.copy}}, \code{\link[base]{getwd}}
#' 
#' @export
dta_get_datasets <- function(copy_to_file_path = NULL) {
  if (is.null(copy_to_file_path)) {
    copy_to_file_path <- file.path(getwd(), "dta-data.xlsx")
  }

  request <- file.copy(dta_path(), copy_to_file_path)
  if (!request) {
    cat(paste0("File successfully exported to '", copy_to_file_path, "'"))
  } else {
    stop("Export request failed")
  }
}


#' Get the path to the example dataset
#'
#' \code{dta_path()} provides the file path to the example dataset
#' \code{dta-data.xlsx} included in the package. The dataset is located in the
#' \code{extdata} directory of the package.
#'
#' @return A string containing the full file path to the dataset within the
#'         package. If the file does not exist, it returns an empty string.
#'
#' @details
#' The function uses \code{\link[base]{system.file}} to construct the path to
#' the \code{extdata/dta-data.xlsx} file included in the package. This is
#' useful for accessing built-in example files.
#'
#' @examples
#' # Get the file path to the dataset
#' # dta_path()
#'
#' @seealso \code{\link[base]{system.file}}
#' 
#' @export
dta_path <- function() {
  system.file("extdata", "dta-data.xlsx", package = "dta")
}