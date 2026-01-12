# Test a model for outliers

Test a model for outliers

## Usage

``` r
get_test_outliers(x, model_index = 1)
```

## Arguments

- x:

  A dataframe of models from create_models()

- model_index:

  Row number from x to get the outliers from

## Value

Result of DHARMa::testOutliers

## Examples

``` r
if (FALSE) { # \dontrun{
get_residuals(mod_df, 1)
} # }
```
