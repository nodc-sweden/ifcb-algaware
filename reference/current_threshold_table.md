# Threshold adjustment table for the current session

Threshold adjustment table for the current session

## Usage

``` r
current_threshold_table(rv)
```

## Arguments

- rv:

  Reactive values with `thresholds_trained`, `threshold_adjustments` and
  `classifications_original`.

## Value

A data.frame from
[`threshold_summary()`](https://nodc-sweden.github.io/ifcb-algaware/reference/threshold_summary.md),
or `NULL` when no threshold is adjusted.
