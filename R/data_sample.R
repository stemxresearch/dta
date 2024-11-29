#' Sample data for demographic, technology, and employment information.
#'
#' A dataset containing demographic information about individuals, including
#' their unique identifier, age, height, weight, marital status, education
#' level, employment status, socioeconomic status, language, technology
#' ownership, and skills in various tools.
#'
#' @format A tibble with 10 rows and 19 variables:
#' \describe{
#'   \item{id}{\code{character}: Unique identifier for each individual.}
#'   \item{region}{\code{factor}: The region where the individual is from.}
#'   \item{age}{\code{numeric}: Age of the individual in years.}
#'   \item{age_group}{\code{factor}: Age group of the individual.}
#'   \item{height}{\code{numeric}: Height of the individual in meters.}
#'   \item{weight}{\code{numeric}: Weight of the individual in kilograms.}
#'   \item{blood_group}{\code{factor}: Blood group of the individual.}
#'   \item{marital_status}{\code{factor}: Marital status of the individual.}
#'   \item{education}{\code{factor}: The highest level of education attained.}
#'   \item{employed}{\code{factor}: Employment status of the individual.}
#'   \item{ses}{\code{factor}: Socioeconomic status of the individual.}
#'   \item{language}{\code{factor}: The primary language spoken by the
#'   individual.}
#'   \item{phone}{\code{factor}: The type of phone the individual uses.}
#'   \item{transport}{\code{factor}: The primary mode of transport used by the
#'   individual.}
#'   \item{gadgets_owned}{\code{character}: A comma-separated list of gadgets
#'   owned by the individual.}
#'   \item{r}{\code{factor}: Whether the individual has skills in R.}
#'   \item{python}{\code{factor}: Whether the individual has skills in Python.}
#'   \item{sas}{\code{factor}: Whether the individual has skills in SAS.}
#'   \item{stata}{\code{factor}: Whether the individual has skills in Stata.}
#'   \item{spss}{\code{factor}: Whether the individual has skills in SPSS.}
#'   \item{excel}{\code{factor}: Whether the individual has skills in Excel.}
#' }
#'
#' @usage
#' data("data_sample")
#'
#' @examples
#' dta_gt(data_sample)
#'
#' @source Simulated data for demonstration purposes.
"data_sample"
