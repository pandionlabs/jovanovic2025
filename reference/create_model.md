# Build a model and return fit statistics

Build a model and return fit statistics

## Usage

``` r
create_model(variable, model_family, TreMs)
```

## Arguments

- variable:

  Character specifying the y variable for the model

- model_family:

  Model family passed to glmmTMB::glmmTMB

- TreMs:

  A TreMs dataframe

## Value

A tibble containing a model and statistics

## Details

Builds a glmmTMB::glmmTMB model using the formula {variable} ~
GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1\|Plot)
Where the Y variable is selected from the available columns in TreMs

## Examples

``` r
if (FALSE) { # \dontrun{
TreMs <- clean_data(MasterThesisData2024)
richness <- create_model("Richness", "poisson", TreMs)
} # }
```
