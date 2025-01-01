data_gadgets <- readxl::read_excel(path = dta_path(), sheet = "data-gadgets")

usethis::use_data(data_gadgets, overwrite = TRUE)
