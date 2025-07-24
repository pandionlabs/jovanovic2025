
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
| Conifer Log/Entire Tree | 313 | 58.7242026 | 32.56550 | 10.787167 | 20.0 | 73.5 | 6.945687 | 7.1059015 | 86 | 4.265176 | 1.6300370 | 11 |
| Conifer Stump | 194 | 36.3977486 | 49.50670 | 17.510500 | 21.0 | 125.0 | 5.824742 | 2.9887884 | 21 | 4.407216 | 1.4230339 | 9 |
| Broadleaf Stump | 2 | 0.3752345 | 46.15000 | 11.525840 | 38.0 | 54.3 | 7.000000 | 1.4142136 | 8 | 6.000000 | 0.0000000 | 6 |
| No ID Stump | 12 | 2.2514071 | 50.69167 | 18.746416 | 34.8 | 105.0 | 4.583333 | 0.6685579 | 6 | 3.916667 | 0.6685579 | 5 |
| Broadleaf Log/Entire Tree | 7 | 1.3133208 | 32.92857 | 10.320968 | 23.4 | 49.8 | 4.428571 | 3.5050983 | 12 | 3.428571 | 1.3972763 | 6 |
| No ID Log/Entire Tree | 5 | 0.9380863 | 27.36000 | 7.867846 | 20.3 | 39.0 | 4.600000 | 1.3416408 | 6 | 3.800000 | 1.3038405 | 6 |

| DeadwoodIdentitiesGrouped | Freq | Percentage | Treedata.DBH_cm.mean | Treedata.DBH_cm.sd | Treedata.DBH_cm.min | Treedata.DBH_cm.max | Abundance.mean | Abundance.sd | Abundance.max | Richness.mean | Richness.sd | Richness.max |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| P. abies Log/Entire Tree | 192 | 36.0225141 | 31.94740 | 10.352878 | 20.0 | 73.5 | 6.510417 | 5.2247794 | 36 | 4.177083 | 1.6938125 | 11 |
| P. abies Stump | 109 | 20.4502814 | 50.66330 | 19.340280 | 22.5 | 125.0 | 6.082569 | 3.3585944 | 21 | 4.321101 | 1.5207255 | 9 |
| Conifer Stump | 36 | 6.7542214 | 47.50000 | 16.227543 | 24.0 | 80.0 | 5.277778 | 2.5140873 | 15 | 4.388889 | 1.0495653 | 6 |
| Conifer Log/Entire Tree | 26 | 4.8780488 | 32.29615 | 14.722948 | 20.0 | 70.0 | 5.384615 | 3.2382331 | 16 | 4.038462 | 1.2483836 | 7 |
| A. alba Stump | 12 | 2.2514071 | 49.09167 | 12.577719 | 34.0 | 75.0 | 4.500000 | 1.9306146 | 8 | 4.083333 | 1.6764862 | 8 |
| Broadleaf Stump | 2 | 0.3752345 | 46.15000 | 11.525840 | 38.0 | 54.3 | 7.000000 | 1.4142136 | 8 | 6.000000 | 0.0000000 | 6 |
| No ID Stump | 12 | 2.2514071 | 50.69167 | 18.746416 | 34.8 | 105.0 | 4.583333 | 0.6685579 | 6 | 3.916667 | 0.6685579 | 5 |
| Broadleaf Log/Entire Tree | 7 | 1.3133208 | 32.92857 | 10.320968 | 23.4 | 49.8 | 4.428571 | 3.5050983 | 12 | 3.428571 | 1.3972763 | 6 |
| F. sylvatica Stump | 37 | 6.9418386 | 48.18649 | 14.401006 | 21.0 | 79.0 | 6.027027 | 2.3744764 | 12 | 4.783784 | 1.3361457 | 7 |
| No ID Log/Entire Tree | 5 | 0.9380863 | 27.36000 | 7.867846 | 20.3 | 39.0 | 4.600000 | 1.3416408 | 6 | 3.800000 | 1.3038405 | 6 |
| F. sylvatica Log/Entire Tree | 49 | 9.1932458 | 32.52041 | 10.319722 | 20.0 | 57.5 | 8.938776 | 13.3000754 | 86 | 4.469388 | 1.5289163 | 9 |
| A. alba Log/Entire Tree | 46 | 8.6303940 | 35.34565 | 10.404715 | 20.9 | 59.7 | 7.521739 | 5.7299957 | 31 | 4.543478 | 1.6425796 | 7 |
