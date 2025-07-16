
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jovanovic2025

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/jovanovic2025)](https://CRAN.R-project.org/package=jovanovic2025)
[![R-CMD-check](https://github.com/pandionlabs/jovanovic2025/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pandionlabs/jovanovic2025/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This is the companion code and data to Jovanovic et al. 2025.

## Installation

You can install the development version of jovanovic2025 from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("pandionlabs/jovanovic2025")
```

## Example

Load the package like this:

``` r
library(jovanovic2025)
```

``` r
# Import the data from a tab delimited ascii file.
# MasterThesisData <- read.table(
#   file = "data/raw/MasterThesisData2024.csv",
#   header = TRUE,
#   sep = ",",
#   na.strings = "NA",
#   stringsAsFactors = TRUE,
#   dec = "."
# )

# Import the data with MasterThesisData2024
TreMs <- 
  clean_data(MasterThesisData2024) 
```
