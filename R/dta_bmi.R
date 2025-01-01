#' Calculate body mass index (BMI)
#'
#' \code{dta_bmi()} computes the Body Mass Index (BMI) given weight and height
#' columns in a data frame or tibble. It allows customization for weight and
#' height units.
#'
#' @param dat A data frame or tibble containing the weight and height columns.
#' @param .weight Name of the column representing weight in the data.
#' @param .height Name of the column representing height in the data.
#' @param weight_units Character string specifying the units of weight.
#'                     Options are \code{"kg"} for kilograms or \code{"lbs"}
#'                     for pounds. Default is \code{"kg"}.
#' @param height_units Character string specifying the units of height.
#'                     Options are \code{"m"} for meters or \code{"in"} for
#'                     inches. Default is \code{"m"}.
#' @param name Character string specifying the name of the new BMI column.
#'              Default is \code{bmi}.
#' @param digits Integer specifying the number of decimal places for rounding
#'                the BMI value. Default is \code{NULL}.
#'
#' @references
#' \url{https://www.cdc.gov/bmi/adult-calculator/bmi-categories.html}
#'
#' @return A tibble with the calculated body mass index (BMI).
#'
#' @examples
#' data("data_bmi")
#' dta_gtable(data_bmi)
#'
#' # Calculate BMI from the columns `weight` and `height`
#'
#' df <- dta_bmi(
#'   dat = data_bmi,
#'   .weight = weight,
#'   .height = height
#' )
#' dta_gtable(df)
#'
#' # Calculate BMI from the columns `weight` and `height`
#' # to 2 decimal points and assign the values to the new
#' # variable named `body_mass_index`
#'
#' df2 <- dta_bmi(
#'   dat = data_bmi,
#'   .weight = weight,
#'   .height = height,
#'   name = body_mass_index,
#'   digits = 2
#' )
#' dta_gtable(df2)
#'
#' @export
dta_bmi <- function(
  dat, .weight, .height, weight_units = c("kg", "lbs"),
  height_units = c("m", "in"), name = "bmi", digits = NULL
) {

  dat <- tibble::as_tibble(
    check_dataframe_or_tibble(dat = dat, par_name = "dat")
  )
  weight_col <- dplyr::pull(.data = dat, var = {{ .weight }})
  height_col <- dplyr::pull(.data = dat, var = {{ .height }})
  weight_units <- tolower(match.arg(weight_units))[1]
  height_units <- tolower(match.arg(height_units))[1]

  is_kgs <- grepl("^[lp]", weight_units)
  is_metres <- grepl("^[i]", height_units)
  weight_new <- if (is_kgs) weight_col * 0.453592 else weight_col
  height_new <- if (is_metres) height_col * 0.0254 else height_col
  digits <- ifelse(is.null(digits), 16, digits)
  name_col <- rlang::ensym(name)
  dat[[name_col]] <- round(weight_new / (height_new ^ 2), digits = digits)

  return(dat)
}


#' Categorize BMI into weight categories
#'
#' \code{dta_bmicat()} categorizes Body Mass Index (BMI) values into specified
#' weight categories.
#'
#' @param dat A data frame or tibble containing the BMI data.
#' @param .bmi The name of the column in \code{dat} containing BMI values.
#' @param name A character string specifying the name of the new column to be
#'              created with BMI categories. Default is \code{bmi_cat}.
#' @param is_extended A logical value indicating whether to use the extended
#'                    BMI categories. If \code{TRUE}, includes Class 1, 2,
#'                    and 3 obesity. Default is \code{FALSE}. See Details
#'                    section for more information.
#' @param out_of_range A character string specifying the label for out-of-range
#'                     BMI values. Default is an empty string.
#' @param as_factor A logical value indicating whether to return the BMI
#'                  categories as a factor. Default is \code{TRUE}.
#' @param as_numeric A logical value indicating whether to convert the factor
#'                   to numeric. Only applicable if \code{as_factor} is
#'                   \code{TRUE}. Default is \code{FALSE}.
#'
#' @details
#' The standard BMI categories are defined as follows:
#'
#' \describe{
#'   \item{Underweight}{BMI < 18.5}
#'   \item{Healthy weight}{18.5 ≤ BMI < 25.0}
#'   \item{Overweight}{25.0 ≤ BMI < 30.0}
#'   \item{Obesity}{BMI ≥ 30.0}
#' }
#'
#' If \code{is_extended} is set to \code{TRUE}, the extended BMI categories are:
#'
#' \describe{
#'   \item{Underweight}{BMI < 18.5}
#'   \item{Healthy weight}{18.5 ≤ BMI < 25.0}
#'   \item{Overweight}{25.0 ≤ BMI < 30.0}
#'   \item{Class I Obesity}{30.0 ≤ BMI < 35.0}
#'   \item{Class II Obesity}{35.0 ≤ BMI < 40.0}
#'   \item{Class III Obesity (Severe)}{BMI ≥ 40.0}
#' }
#'
#' @return A data frame or tibble with an additional column for BMI categories.
#'
#' @examples
#' data("data_bmicat")
#' dta_gtable(data_bmicat)
#'
#' # Categorize `bmi` into the standard BMI categories
#'
#' df <- dta_bmicat(
#'   dat = data_bmicat,
#'   .bmi = bmi,
#'   name = bmi_cat,
#'   is_extended = FALSE,
#'   as_factor = TRUE
#' )
#' dta_gtable(df)
#'
#' # Categorize `bmi` into the extended BMI categories
#'
#' df2 <- dta_bmicat(
#'   dat = data_bmicat,
#'   .bmi = bmi,
#'   name = bmi_cat,
#'   is_extended = TRUE,
#'   as_factor = TRUE
#' )
#' dta_gtable(df2)
#'
#' # Categorize `bmi` into the standard BMI categories and
#' # convert to numeric
#'
#' df3 <- dta_bmicat(
#'   dat = data_bmicat,
#'   .bmi = bmi,
#'   name = bmi_cat,
#'   is_extended = TRUE,
#'   as_factor = TRUE,
#'   as_numeric = TRUE
#' )
#' dta_gtable(df3)
#'
#' @seealso \code{\link{dta_bmi}}, \code{\link[dplyr]{case_match}}, \code{\link[dplyr]{case_when}}
#'
#' @export
dta_bmicat <- function(
  dat, .bmi, name = "bmi_cat", is_extended = FALSE, out_of_range = "",
  as_factor = TRUE, as_numeric = FALSE
) {

  dat <- check_dataframe_or_tibble(dat = dat, par_name = "dat")
  bmi_col <- dplyr::pull(.data = dat, var = {{ .bmi }})
  name_col <- rlang::ensym(name)
  if (!is_extended) {
    bmi_cats <- c("Underweight", "Healthy weight", "Overweight", "Obesity")
    bmicat_col <- dplyr::case_when(
      bmi_col < 18.5 ~ bmi_cats[1],
      bmi_col < 25 ~ bmi_cats[2],
      bmi_col < 30 ~ bmi_cats[3],
      bmi_col >= 30 ~ bmi_cats[4],
      TRUE ~ out_of_range
    )
    dat[[name_col]] <- bmicat_col
  } else {
    bmi_cats <- c(
      "Underweight", "Healthy weight", "Overweight", "Class 1 Obesity",
      "Class 2 Obesity", "Class 3 Obesity (Severe)"
    )
    bmicat_col <- dplyr::case_when(
      bmi_col < 18.5 ~ bmi_cats[1],
      bmi_col < 25 ~ bmi_cats[2],
      bmi_col < 30 ~ bmi_cats[3],
      bmi_col < 35 ~ bmi_cats[4],
      bmi_col < 40 ~ bmi_cats[5],
      bmi_col >= 40 ~ bmi_cats[6],
      TRUE ~ out_of_range
    )
    dat[[name_col]] <- bmicat_col
  }

  if (as_factor) {
    dat[[name_col]] = factor(dat[[name_col]], levels = bmi_cats, ordered = TRUE)

    if (as_numeric) {
      dat[[name_col]] <- as.numeric(dat[[name_col]])
    }
  }

  tibble::as_tibble(dat)
}
