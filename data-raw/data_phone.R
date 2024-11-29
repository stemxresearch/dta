set.seed(54321)
options <- c(
  "Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"
)
phone_types <- c("Apple", "Sumsung", "Google", "OnePlus")
n = 15
data_phone <- tibble::tibble(
  id = 1:n,
  phone_type = sample(x = phone_types, size = n, replace = TRUE),
  ease_of_use = sample(options, size = n, replace = TRUE),
  battery_life = sample(options, size = n, replace = TRUE),
  camera_quality = sample(options, size = n, replace = TRUE),
  value_for_money = sample(options, size = n, replace = TRUE),
  design_and_appearance = sample(options, size = n, replace = TRUE)
)

usethis::use_data(data_phone, overwrite = TRUE)
