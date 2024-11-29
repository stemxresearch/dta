#' @noRd
dta_read <- function(data_or_path, sheet = 1, text_delim = "\t") {

  if (is.data.frame(data_or_path) || tibble::is_tibble(data_or_path)) {
    dat <- data_or_path
  } else if (is.character(data_or_path) && length(data_or_path) == 1) {

    file_ext <- tools::file_ext(data_or_path)

    if (file_ext == "csv") {
      dat <- readr::read_csv(data_or_path)

    } else if (file_ext %in% c("xlsx", "xls")) {
      is_valid_sheet <- !is.null(sheet) && length(sheet) == 1 &&
        (is.character(sheet) || is.numeric(sheet) &&
           as.integer(sheet) == sheet)
      sheet <- ifelse(is_valid_sheet, sheet, 1)
      dat <- readxl::read_excel(data_or_path, sheet = sheet)

    } else if (file_ext == "rda") {
      load(data_or_path)
      dat <- ls()

    } else if (toupper(file_ext) == "RDS") {
      dat <- readRDS(data_or_path)

    } else if (file_ext == "dta") {
      dat <- haven::read_dta(data_or_path)
      dat <- tibble::as_tibble(
        lapply(dat, function(x) {
          if (haven::is.labelled(x)) haven::as_factor(x) else x
        })
      )

    } else if (file_ext == "sav") {
      dat <- haven::read_sav(data_or_path)

    } else if (file_ext == "txt") {
      dat <- readr::read_delim(data_or_path, delim = text_delim)

    } else {
      file_formats <- c(
        ".csv", ".xlsx", ".xls", ".rda", ".RDS", ".dta", ".sav", ".txt"
      )
      stop("'", file_ext, "' is an unsupported file format. Valid file ",
           "formats: ", paste0(file_formats, collapse = ", "))
    }
  } else {
    stop("Expected 'data_or_path' to be a DataFrame, tibble or valid file ",
         "path but got ", check_stop(data_or_path))
  }

  tibble::as_tibble(dat)
}

