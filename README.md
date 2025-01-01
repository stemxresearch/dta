
## Overview

`dta` provides simple and efficient tools for data management and
manipulation, including functions to clean, transform, and format
datasets using popular data-handling methods. It aims to simplify common
data-wrangling tasks. These include:

- `dta_bmi()` computes the Body Mass Index (BMI) given weight and height
  columns in a data frame or tibble.
- `dta_freq_mrq()` Frequency table for multiple response questions.
- `dta_mrq()` Split multiple response question column into binary
  columns.
- `dta_recode_auto()` Automatically recode categorical variables in a
  data frame.
- `dta_recode()` Recode variables in a data frame based on a dictionary.
- `dta_transpose()` Transpose a data frame with specified column as
  variable names.

Among others. The full list of functions is available on the
[documentation](https://stemxresearch.github.io/dta/reference/index.html)

## Installation

``` r
install.packages("devtools")
devtools::install_github("stemxresearch/dta")
```

## Usage

To use the `dta` package, you must start by loading it using the
`library()` function as shown below.

``` r
library(dta)
```

## Getting started

Click the **Get Started** link on the top menu or follow the link
[Getting started](https://stemxresearch.github.io/dta/articles/dta.html)
for a quick tutorial on the `dta` library.

## Documentation

The **Reference** menu on the top menu or [this
page](https://stemxresearch.github.io/dta/reference/index.html) provides
a list of all the functions available in the `dta` package with help
documentation including syntax and examples.

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/stemxresearch/dta/issues).

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://www.r-project.org/coc-policy.html). By participating in
this project you agree to abide by its terms.
