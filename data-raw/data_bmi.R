data_bmi <- readxl::read_excel(path = dta_path(), sheet = "data-bmi")

usethis::use_data(data_bmi, overwrite = TRUE)
