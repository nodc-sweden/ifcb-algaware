# Format threshold adjustments for the report summary table

Format threshold adjustments for the report summary table

## Usage

``` r
format_threshold_adjustments(thresholds)
```

## Arguments

- thresholds:

  Data.frame from
  [`threshold_summary()`](https://nodc-sweden.github.io/ifcb-algaware/reference/threshold_summary.md),
  or `NULL`.

## Value

A single string such as `"Unicells (0.72 -> 0.82)"`, or `NULL` when
there are no adjustments.
