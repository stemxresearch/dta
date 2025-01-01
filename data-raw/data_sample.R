set.seed(54321)

n = 2500

dat <- function(n = 2500) {
  size = 250000
  age <- round(rnorm(size, mean = 47, sd = 17))
  height <- round(rnorm(size, mean = 1.65, sd = 9.66), 2)
  weight <- round(rnorm(size, mean = 71, sd = 15))
  bmi <- weight / (height ^ 2)
  dat <- tibble::tibble(age, height, weight, bmi)
  dat <- dplyr::filter(
    dat,
    dplyr::between(age, 20, 80),
    dplyr::between(height, 1.35, 2.00),
    dplyr::between(weight, 30, 120),
    dplyr::between(bmi, 12, 46)
  )
  dat <- dplyr::slice(dat, 1:n)
  dat <- dplyr::select(dat, age, height, weight)

  return(dat)
}

id <- paste0("STM/", sample(x = 4000:8000, size = n, replace = FALSE))

region <- sample(
  x = c("South", "West", "North East", "Central"),
  size = n,
  replace = TRUE,
  prob = c(35, 20, 30, 15)
)

age <- dat(n = n)$age

age_group <- cut(
  age,
  breaks = c(19, 29, 39, 49, 59, 69, Inf),
  labels = c("20-29", "30-39", "40-49", "50-59", "60-69", "70+"),
  right = FALSE
)
age_group <- as.character(age_group)

height <- dat(n = n)$height

weight <- dat(n = n)$weight

blood_group <- sample(
  x = c("A", "B", "AB", "O"),
  size = n,
  replace = TRUE,
  prob = c(23, 27, 38, 12)
)

marital_status <- sample(
  x = c("Single", "Married", "Other"),
  size = n,
  replace = TRUE,
  prob = c(35, 60, 5)
)

education <- sample(
  x = c("Bachelors", "Masters", "Doctorate"),
  size = n,
  replace = TRUE,
  prob = c(50, 30, 20)
)

employed <-  sample(
  x = c("No", "Yes"),
  size = n,
  replace = TRUE,
  prob = c(30, 70)
)

ses <- sample(
  x = c("Low", "Middle", "High"),
  size = n,
  replace = TRUE,
  prob = c(35, 45, 20)
)

language <- sample(
  x = c("English", "Spanish", "Mandarin", "French", "Arabic", "Other"),
  size = n,
  replace = TRUE,
  prob = c(28, 10, 19, 22, 15, 6)
)

phone <- sample(
  x = c("None", "Apple", "Samsung", "Google", "OnePlus", "Xiaomi", "Other"),
  size = n,
  replace = TRUE,
  prob = c(4, 9, 24, 15, 18, 23, 7)
)

transport <- sample(
  x = c("Walking", "Bicycle", "Car", "Bus", "Train"),
  size = n,
  replace = TRUE,
  prob = c(13, 18, 34, 23, 12)
)

demographics <- data.frame(
  id, region, age, age_group, height, weight, blood_group, marital_status,
  education, employed, ses, language, phone, transport
)

# multiple response questions
# ---------------------------

gadgets <- c(
  "Smartphone", "Laptop", "Tablet", "Desktop Computer", "Smart TV",
  "Digital Camera", "Smartwatch"
)

# Function to randomly select a subset of gadgets for each respondent
simulate_responses_vectorized <- function(options, num_respondents) {
  # gadgets for each respondent
  num_selected <- sample(x = 1:5, size = num_respondents, replace = TRUE)

  # create a list of selected gadgets for each respondent
  lapply(
    num_selected, function(n) sample(x = options, size = n, replace = FALSE)
  )
}

# Simulate responses for 50 respondents using the vectorized approach
responses <- simulate_responses_vectorized(
  options = gadgets, num_respondents = n
)

mrq_gadgets <- data.frame(
  gadgets_owned = sapply(responses, function(r) paste(r, collapse = ", "))
)

# programming languages
# ---------------------

languages <- c("R", "Python", "SAS", "Stata", "SPSS", "Excel")

# Function to simulate Yes/No responses for each programming language
simulate_languages_responses <- function(options, num_respondents) {
  # Generate a matrix of random Yes/No responses
  responses_matrix <- matrix(
    sample(
      x = c("No", "Yes"),
      size = num_respondents * length(options),
      replace = TRUE
    ),
    nrow = num_respondents,
    dimnames = list(NULL, options)
  )
  as.data.frame(responses_matrix)
}

languages <- simulate_languages_responses(languages, n)

data_sample <- cbind(demographics, mrq_gadgets, languages)
data_sample <- janitor::clean_names(data_sample)

labels <- c(
  "Unique identifier",
  "Region of residence",
  "Age in years",
  "Age group category",
  "Height in meters",
  "Weight in kilograms",
  "Blood group type",
  "Marital status",
  "Highest education level",
  "Employment status",
  "Socio-economic status",
  "Language spoken",
  "Phone ownership status",
  "Main mode of transport",
  "Types of gadgets owned",
  "R",
  "Python",
  "SAS",
  "Stata",
  "SPSS",
  "Microsoft Excel"
)
data_sample <- labelled::set_variable_labels(
  .data = data_sample, .labels = labels, .strict = TRUE
)

usethis::use_data(data_sample, overwrite = TRUE)
