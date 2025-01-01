data_cancer <- readxl::read_excel(path = dta_path(), sheet = "data-cancer")

usethis::use_data(data_cancer, overwrite = TRUE)
