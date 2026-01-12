# Test a model for zero inflation

Test a model for zero inflation

## Usage

``` r
get_test_zero_inflation(x, model_index = 1)
```

## Arguments

- x:

  A dataframe of models from create_models()

- model_index:

  Row number from x to get the zero inflation test result from

## Value

Result of DHARMa::testZeroInflation

## Examples

``` r
if (FALSE) { # \dontrun{
get_test_zero_inflation(mod_df, 1)
} # }
```
