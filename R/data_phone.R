#' Sample data for phone satistifaction feedback.
#'
#' A dataset containing reviews of various phone models, including user ratings
#' on ease of use, battery life, camera quality, value for money, and design
#' and appearance.
#'
#' @format A tibble with 15 rows and 6 variables:
#' \describe{
#'   \item{id}{\code{numeric}: Unique identifier for each review.}
#'   \item{phone_type}{\code{factor}: The phone model (OnePlus, Sumsung,
#'   Google, Apple).}
#'   \item{ease_of_use}{\code{factor}: Rating for ease of use.}
#'   \item{battery_life}{\code{factor}: Rating for battery life.}
#'   \item{camera_quality}{\code{factor}: Rating for camera quality.}
#'   \item{value_for_money}{\code{factor}: Rating for value for money.}
#'   \item{design_and_appearance}{\code{factor}: Rating for design and appearance.}
#' }
#'
#' @usage
#' data("data_phone")
#'
#' @examples
#' dta_gt(data_phone)
#'
#' @source Simulated data for demonstration purposes.
"data_phone"