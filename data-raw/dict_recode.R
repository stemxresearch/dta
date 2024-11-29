dict_recode <- readxl::read_excel(path = dta_path(), sheet = "dict-recode")

usethis::use_data(dict_recode, overwrite = TRUE)
