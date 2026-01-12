# Summarizes microhabitats in the TreMs dataset

Combines single type of microhabitat observations into larger groups

## Usage

``` r
summarize_microhabitats(TreMs)
```

## Arguments

- TreMs:

  a dataframe from load_data

## Value

A dataframe of summarized microhabitat data

## Examples

``` r
if (FALSE) { # \dontrun{
  TreMs <- load_data(MasterThesisData2024)
  TreMs <- summarize_microhabitats(TreMs)
} # }
```
