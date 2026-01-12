# Get model simulated residuals

Get model simulated residuals

## Usage

``` r
get_residuals(x, model_index = 1)
```

## Arguments

- x:

  A dataframe of models from create_models()

- model_index:

  Row number from x to get the residuals from

## Value

Model simulated residuals from DHARMa::simulateResiduals

## Examples

``` r
if (FALSE) { # \dontrun{
get_residuals(mod_df, 1)
} # }
```
