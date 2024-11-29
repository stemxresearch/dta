dict_misspelled <- readxl::read_excel(
  path = dta_path(), sheet = "dict-misspelled"
)

usethis::use_data(dict_misspelled, overwrite = TRUE)
