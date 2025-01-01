data_cancer2 <- readxl::read_excel(path = dta_path(), sheet = "data-cancer2")

usethis::use_data(data_cancer2, overwrite = TRUE)
