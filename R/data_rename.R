#' Sample data for demographic and socioeconomic information.
#'
#' A dataset containing demographic information about individuals, including
#' their unique identifier, age, height, weight, marital status, education
#' level, employment status, and socioeconomic status.
#'
#' @format A tibble with 15 rows and 9 variables:
#' \describe{
#'   \item{uniqueidentifier}{\code{character}: Unique identifier for each
#'   individual.}
#'   \item{where_from}{\code{character}: The region where the individual is
#'   from.}
#'   \item{age}{\code{numeric}: Age of the individual in years.}
#'   \item{height}{\code{numeric}: Height of the individual in meters.}
#'   \item{weight}{\code{numeric}: Weight of the individual in kilograms.}
#'   \item{group}{\code{character}: Blood type group of the individual.}
#'   \item{maritalstatus}{\code{character}: Marital status of the individual.}
#'   \item{highesteducationlevel}{\code{character}: The highest education
#'   level attained by the individual.}
#'   \item{employed}{\code{character}: Employment status of the individual.}
#'   \item{socioeconomicstatus}{\code{character}: The socioeconomic status of
#'   the individual.}
#' }
#'
#' @usage
#' data("data_rename")
#'
#' @examples
#' dta_gtable(data_rename)
#'
#' @source Simulated data for demonstration purposes.
"data_rename"
