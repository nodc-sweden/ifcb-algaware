# Turn imported threshold rows into threshold adjustments

A row is only applied when its class exists in the loaded classifier and
its trained threshold matches the loaded one, so thresholds saved for a
different classifier are never applied. Rows that are skipped are
reported with the reason.

## Usage

``` r
adjustments_from_import(threshold_rows, trained, tolerance = 1e-06)
```

## Arguments

- threshold_rows:

  Threshold rows from
  [`split_corrections_import()`](https://nodc-sweden.github.io/ifcb-algaware/reference/split_corrections_import.md),
  or `NULL`.

- trained:

  Named numeric vector of trained thresholds, or `NULL` when the loaded
  files have none.

- tolerance:

  Allowed difference between the file's and the loaded trained
  thresholds.

## Value

A list with `adjustments` (named numeric vector) and `skipped`
(data.frame with `class_name` and `reason`).
