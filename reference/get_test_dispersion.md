# Test a model for dispersion

Test a model for dispersion

## Usage

``` r
get_test_dispersion(x, model_index = 1)
```

## Arguments

- x:

  A dataframe of models from create_models()

- model_index:

  Row number from x to get the dispersion test result from

## Value

Result of DHARMa::testDispersion

## Examples

``` r
if (FALSE) { # \dontrun{
get_test_dispersion(mod_df, 1)
} # }
```
