# Group data by species and deadwood type

Group data by species and deadwood type

## Usage

``` r
group_data(TreMs)
```

## Arguments

- TreMs:

  A dataframe from summarize_microhabitats

## Value

A dataframe

## Examples

``` r
if (FALSE) { # \dontrun{
  TreMs <- load_data(MasterThesisData2024)
  TreMs <- summarize_microhabitats(TreMs)
  TreMs <- group_data(TreMs)
} # }
```
