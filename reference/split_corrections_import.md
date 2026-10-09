# Split an imported corrections file into corrections and thresholds

Files written before threshold adjustments existed have no `record_type`
column; all their rows are corrections.

## Usage

``` r
split_corrections_import(df)
```

## Arguments

- df:

  Data.frame read from a corrections CSV.

## Value

A list with `corrections` and `thresholds` data.frames.
