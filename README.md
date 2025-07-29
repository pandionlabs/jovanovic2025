
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
| Broadleaf Log/Entire Tree | 7 | 1.3133208 | 32.92857 | 10.320968 | 23.4 | 49.8 | 4.571429 | 3.5050983 | 12 | 3.571429 | 1.5118579 | 6 |
| Broadleaf Stump | 2 | 0.3752345 | 46.15000 | 11.525840 | 38.0 | 54.3 | 7.500000 | 0.7071068 | 8 | 6.500000 | 0.7071068 | 7 |
| Conifer Log/Entire Tree | 313 | 58.7242026 | 32.56550 | 10.787167 | 20.0 | 73.5 | 7.453674 | 7.2817779 | 86 | 4.578275 | 1.8983227 | 12 |
| Conifer Stump | 194 | 36.3977486 | 49.50670 | 17.510500 | 21.0 | 125.0 | 6.634021 | 3.3739443 | 22 | 4.917526 | 1.7074667 | 10 |
| No ID Log/Entire Tree | 5 | 0.9380863 | 27.36000 | 7.867846 | 20.3 | 39.0 | 4.600000 | 1.3416408 | 6 | 3.800000 | 1.3038405 | 6 |
| No ID Stump | 12 | 2.2514071 | 50.69167 | 18.746416 | 34.8 | 105.0 | 5.500000 | 1.5666989 | 9 | 4.500000 | 1.0871146 | 6 |

| DeadwoodIdentitiesGrouped | Freq | Percentage | Treedata.DBH_cm.mean | Treedata.DBH_cm.sd | Treedata.DBH_cm.min | Treedata.DBH_cm.max | Abundance.mean | Abundance.sd | Abundance.max | Richness.mean | Richness.sd | Richness.max |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| A. alba Log/Entire Tree | 46 | 8.6303940 | 35.34565 | 10.404715 | 20.9 | 59.7 | 8.043478 | 5.8687743 | 31 | 4.826087 | 1.8415494 | 8 |
| A. alba Stump | 12 | 2.2514071 | 49.09167 | 12.577719 | 34.0 | 75.0 | 5.666667 | 3.0251471 | 11 | 4.666667 | 2.0150946 | 9 |
| Broadleaf Log/Entire Tree | 7 | 1.3133208 | 32.92857 | 10.320968 | 23.4 | 49.8 | 4.571429 | 3.5050983 | 12 | 3.571429 | 1.5118579 | 6 |
| Broadleaf Stump | 2 | 0.3752345 | 46.15000 | 11.525840 | 38.0 | 54.3 | 7.500000 | 0.7071068 | 8 | 6.500000 | 0.7071068 | 7 |
| Conifer Log/Entire Tree | 26 | 4.8780488 | 32.29615 | 14.722948 | 20.0 | 70.0 | 5.923077 | 4.0784612 | 17 | 4.269231 | 1.5114944 | 8 |
| Conifer Stump | 36 | 6.7542214 | 47.50000 | 16.227543 | 24.0 | 80.0 | 5.944444 | 2.6826900 | 15 | 4.916667 | 1.3174651 | 7 |
| F. sylvatica Log/Entire Tree | 49 | 9.1932458 | 32.52041 | 10.319722 | 20.0 | 57.5 | 9.367347 | 13.3474309 | 86 | 4.795918 | 1.8025398 | 10 |
| F. sylvatica Stump | 37 | 6.9418386 | 48.18649 | 14.401006 | 21.0 | 79.0 | 7.513514 | 3.1852230 | 16 | 5.594595 | 1.4806053 | 8 |
| No ID Log/Entire Tree | 5 | 0.9380863 | 27.36000 | 7.867846 | 20.3 | 39.0 | 4.600000 | 1.3416408 | 6 | 3.800000 | 1.3038405 | 6 |
| No ID Stump | 12 | 2.2514071 | 50.69167 | 18.746416 | 34.8 | 105.0 | 5.500000 | 1.5666989 | 9 | 4.500000 | 1.0871146 | 6 |
| P. abies Log/Entire Tree | 192 | 36.0225141 | 31.94740 | 10.352878 | 20.0 | 73.5 | 7.031250 | 5.4800029 | 36 | 4.505208 | 1.9815838 | 12 |
| P. abies Stump | 109 | 20.4502814 | 50.66330 | 19.340280 | 22.5 | 125.0 | 6.669725 | 3.6287306 | 22 | 4.715596 | 1.8160383 | 10 |

## Make a count table by decay stage

``` r
count_table <- make_count_table_by_decay(
  TreMs, 
  save_name = "data/derivatives/Count_Table_by_Decay_Stage.csv"
)
```

## Create models

In the create models section, we build a series of models associating
various predictor (y) variables with three varialbes:
`GroupedTreeSpecies, Treedata.DBH_cm, and Treedata.Tree_Decay`. The
modelling family for each model is either poisson or glmmTMB::nbinom2.

The `get_model_cols()` function creates a tibble that specifies each
predictor variable and the model family used in each model

``` r
model_parameters <- get_model_cols()
```

| variable | model_family | model_formula |
|:---|:---|:---|
| DecomposedCrack | poisson | DecomposedCrack ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| WoodpeckerCavities | poisson | WoodpeckerCavities ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| Concavities | poisson | Concavities ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| WoodpeckerConcavities | poisson | WoodpeckerConcavities ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| Rotholes | poisson | Rotholes ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| InsectGalleries | poisson | InsectGalleries ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| ExposedSapwood | glmmTMB::nbinom2 | ExposedSapwood ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| ExposedHeartwood | glmmTMB::nbinom2 | ExposedHeartwood ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| ExposedSapwoodHeartwood | glmmTMB::nbinom2 | ExposedSapwoodHeartwood ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| PerennialFungi | glmmTMB::nbinom2 | PerennialFungi ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| Ephermalfungi | glmmTMB::nbinom2 | Ephermalfungi ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| EphrmalPerennialFungi | glmmTMB::nbinom2 | EphrmalPerennialFungi ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| Epiphytes | glmmTMB::nbinom2 | Epiphytes ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| DeadwooodShelter | glmmTMB::nbinom2 | DeadwooodShelter ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| StumpStructures | poisson | StumpStructures ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| DeadwooodShelterStumpStructures | glmmTMB::nbinom2 | DeadwooodShelterStumpStructures ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| LogStructures | glmmTMB::nbinom2 | LogStructures ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| WoodyDebris | glmmTMB::nbinom2 | WoodyDebris ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| ExposedRoots | glmmTMB::nbinom2 | ExposedRoots ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| Abundance | glmmTMB::nbinom2 | Abundance ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |
| Richness | glmmTMB::nbinom2 | Richness ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1&#124;Plot) |

The models can be produced with `create_models()` which will create each
of the models specified in `get_model_cols()` and then save the model
summary, residuals, and results from three tests of outliers,
dispersion, and zero inflation. All these models are results are
returned as a dataframe.

``` r
mod_df <- create_models(TreMs)
```
