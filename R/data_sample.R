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
#'   \item{region}{\code{character}: The region where the individual is from.}
#'   \item{age}{\code{numeric}: Age of the individual in years.}
#'   \item{age_group}{\code{character}: Age group of the individual.}
#'   \item{height}{\code{numeric}: Height of the individual in meters.}
#'   \item{weight}{\code{numeric}: Weight of the individual in kilograms.}
#'   \item{blood_group}{\code{character}: Blood group of the individual.}
#'   \item{marital_status}{\code{character}: Marital status of the individual.}
#'   \item{education}{\code{character}: The highest level of education attained.}
#'   \item{employed}{\code{character}: Employment status of the individual.}
#'   \item{ses}{\code{character}: Socioeconomic status of the individual.}
#'   \item{language}{\code{character}: The primary language spoken by the
#'   individual.}
#'   \item{phone}{\code{character}: The type of phone the individual uses.}
#'   \item{transport}{\code{character}: The primary mode of transport used by the
#'   individual.}
#'   \item{gadgets_owned}{\code{character}: A comma-separated list of gadgets
#'   owned by the individual.}
#'   \item{r}{\code{character}: Whether the individual has skills in R.}
#'   \item{python}{\code{character}: Whether the individual has skills in Python.}
#'   \item{sas}{\code{character}: Whether the individual has skills in SAS.}
#'   \item{stata}{\code{character}: Whether the individual has skills in Stata.}
#'   \item{spss}{\code{character}: Whether the individual has skills in SPSS.}
#'   \item{excel}{\code{character}: Whether the individual has skills in Excel.}
#' }
#'
#' @usage
#' data("data_sample")
#'
#' @examples
#' dta_gtable(head(data_sample))
#'
#' @source Simulated data for demonstration purposes.
"data_sample"
