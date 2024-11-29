data_rename <- readxl::read_excel(path = dta_path(), sheet = "data-rename")

usethis::use_data(data_rename, overwrite = TRUE)
