
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
TreMs <- clean_data(MasterThesisData2024) 
```

``` r


TreeIdentitiesSummary <- summarize_identities(
  TreMs, 
  grouping_variable = TreeIdentities2
  )

  # write.table(TreeIdentitiesSummary, file = "data/derivatives/TreeIdentitiesSummary.csv", sep = ",", quote = FALSE, row.names = FALSE)


DeadwoodIdentitiesGroupedSummary <- summarize_identities(
  TreMs, 
  grouping_variable = DeadwoodIdentitiesGrouped
  )

# write.table(DeadwoodIdentitiesGroupedSummary, file = "data/derivatives/DeadwoodIdentitiesGroupedSummary.csv", sep = ",", quote = FALSE, row.names = FALSE)
```

| TreeIdentities2 | Freq | Percentage | Treedata.DBH_cm.mean | Treedata.DBH_cm.sd | Treedata.DBH_cm.min | Treedata.DBH_cm.max | Abundance.mean | Abundance.sd | Abundance.max | Richness.mean | Richness.sd | Richness.max |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Broadleaf Log/Entire Tree | 7 | 1.3133208 | 32.92857 | 10.320968 | 23.4 | 49.8 | 3427 | 0 | 3427 | 2292 | 0 | 2292 |
| Broadleaf Stump | 2 | 0.3752345 | 46.15000 | 11.525840 | 38.0 | 54.3 | 3427 | 0 | 3427 | 2292 | 0 | 2292 |
| Conifer Log/Entire Tree | 313 | 58.7242026 | 32.56550 | 10.787167 | 20.0 | 73.5 | 3427 | 0 | 3427 | 2292 | 0 | 2292 |
| Conifer Stump | 194 | 36.3977486 | 49.50670 | 17.510500 | 21.0 | 125.0 | 3427 | 0 | 3427 | 2292 | 0 | 2292 |
| No ID Log/Entire Tree | 5 | 0.9380863 | 27.36000 | 7.867846 | 20.3 | 39.0 | 3427 | 0 | 3427 | 2292 | 0 | 2292 |
| No ID Stump | 12 | 2.2514071 | 50.69167 | 18.746416 | 34.8 | 105.0 | 3427 | 0 | 3427 | 2292 | 0 | 2292 |

| DeadwoodIdentitiesGrouped | Freq | Percentage | Treedata.DBH_cm.mean | Treedata.DBH_cm.sd | Treedata.DBH_cm.min | Treedata.DBH_cm.max | Abundance.mean | Abundance.sd | Abundance.max | Richness.mean | Richness.sd | Richness.max |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| A. alba Log/Entire Tree | 46 | 8.6303940 | 35.34565 | 10.404715 | 20.9 | 59.7 | 3427 | 0 | 3427 | 2292 | 0 | 2292 |
| A. alba Stump | 12 | 2.2514071 | 49.09167 | 12.577719 | 34.0 | 75.0 | 3427 | 0 | 3427 | 2292 | 0 | 2292 |
| Broadleaf Log/Entire Tree | 7 | 1.3133208 | 32.92857 | 10.320968 | 23.4 | 49.8 | 3427 | 0 | 3427 | 2292 | 0 | 2292 |
| Broadleaf Stump | 2 | 0.3752345 | 46.15000 | 11.525840 | 38.0 | 54.3 | 3427 | 0 | 3427 | 2292 | 0 | 2292 |
| Conifer Log/Entire Tree | 26 | 4.8780488 | 32.29615 | 14.722948 | 20.0 | 70.0 | 3427 | 0 | 3427 | 2292 | 0 | 2292 |
| Conifer Stump | 36 | 6.7542214 | 47.50000 | 16.227543 | 24.0 | 80.0 | 3427 | 0 | 3427 | 2292 | 0 | 2292 |
| F. sylvatica Log/Entire Tree | 49 | 9.1932458 | 32.52041 | 10.319722 | 20.0 | 57.5 | 3427 | 0 | 3427 | 2292 | 0 | 2292 |
| F. sylvatica Stump | 37 | 6.9418386 | 48.18649 | 14.401006 | 21.0 | 79.0 | 3427 | 0 | 3427 | 2292 | 0 | 2292 |
| No ID Log/Entire Tree | 5 | 0.9380863 | 27.36000 | 7.867846 | 20.3 | 39.0 | 3427 | 0 | 3427 | 2292 | 0 | 2292 |
| No ID Stump | 12 | 2.2514071 | 50.69167 | 18.746416 | 34.8 | 105.0 | 3427 | 0 | 3427 | 2292 | 0 | 2292 |
| P. abies Log/Entire Tree | 192 | 36.0225141 | 31.94740 | 10.352878 | 20.0 | 73.5 | 3427 | 0 | 3427 | 2292 | 0 | 2292 |
| P. abies Stump | 109 | 20.4502814 | 50.66330 | 19.340280 | 22.5 | 125.0 | 3427 | 0 | 3427 | 2292 | 0 | 2292 |
