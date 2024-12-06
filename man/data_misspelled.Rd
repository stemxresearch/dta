% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_misspelled.R
\docType{data}
\name{data_misspelled}
\alias{data_misspelled}
\title{Sample dataset with general information.}
\format{
A data frame with 50 rows and 15 variables:
\describe{
\item{id}{\code{character}: Unique identifier for individuals.}
\item{region}{\code{character}: Region of residence.}
\item{age}{\code{numeric}: Age of the individual (in years).}
\item{height}{\code{numeric}: Height of the individual (in meters).}
\item{weight}{\code{numeric}: Weight of the individual (in kilograms).}
\item{blood_group}{\code{character}: Blood group of the individual.}
\item{marital_status}{\code{character}: Marital status of the individual.}
\item{education}{\code{character}: Highest level of education attained.}
\item{ses}{\code{character}: Socioeconomic status.}
\item{r}{\code{character}: Proficiency in R programming.}
\item{python}{\code{character}: Proficiency in Python programming.}
\item{sas}{\code{character}: Proficiency in SAS.}
\item{stata}{\code{character}: Proficiency in Stata.}
\item{spss}{\code{character}: Proficiency in SPSS.}
\item{excel}{\code{character}: Proficiency in Excel.}
}
}
\source{
Simulated data for illustrative purposes.
}
\usage{
data("data_misspelled")
}
\description{
This dataset contains information on individuals, including demographics,
education, skills in various tools, and socioeconomic status.
}
\examples{
dta_gtable(data_misspelled)

}
\keyword{datasets}
