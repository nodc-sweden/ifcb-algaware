# Summarise threshold adjustments for export and reporting

Summarise threshold adjustments for export and reporting

## Usage

``` r
threshold_summary(trained, adjustments, original)
```

## Arguments

- trained:

  Named numeric vector of trained thresholds.

- adjustments:

  Named numeric vector of adjusted thresholds.

- original:

  The load-time classifications.

## Value

A data.frame with `class_name`, `trained`, `adjusted` and `n_moved`
(images whose label the adjustment changes, before manual corrections),
one row per adjusted class.
