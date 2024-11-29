data_bmicat <- readxl::read_excel(path = dta_path(), sheet = "data-bmicat")

usethis::use_data(data_bmicat, overwrite = TRUE)
