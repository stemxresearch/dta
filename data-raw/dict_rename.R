dict_rename <- readxl::read_excel(path = dta_path(), sheet = "dict-rename")

usethis::use_data(dict_rename, overwrite = TRUE)
