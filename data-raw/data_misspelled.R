data_misspelled <- readxl::read_excel(
  path = dta_path(), sheet = "data-misspelled"
)

usethis::use_data(data_misspelled, overwrite = TRUE)
