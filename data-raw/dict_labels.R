dict_labels <- readxl::read_excel(path = dta_path(), sheet = "dict-labels")

usethis::use_data(dict_labels, overwrite = TRUE)
