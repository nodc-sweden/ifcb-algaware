# Apply a corrections log to classifications

Sets `class_name` to `new_class` for every ROI in the corrections log.
When an ROI appears more than once, the last row wins (the log is
chronological). Corrections for ROIs not in `classifications` are
ignored.

## Usage

``` r
apply_corrections(classifications, corrections)
```

## Arguments

- classifications:

  A data.frame with `sample_name`, `roi_number` and `class_name`
  columns.

- corrections:

  A data.frame with `sample_name`, `roi_number` and `new_class` columns,
  or `NULL`.

## Value

A data.frame like `classifications` with corrected `class_name`.
