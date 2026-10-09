# Build the corrections CSV export, including threshold adjustments

One file holds everything needed to restore a validation session: the
corrections log (with embedded custom class metadata, see
[`enrich_corrections_for_export()`](https://nodc-sweden.github.io/ifcb-algaware/reference/enrich_corrections_for_export.md))
followed by one row per adjusted class threshold. A `record_type` column
tells the two apart (`"correction"` or `"threshold"`); threshold rows
leave the correction columns empty and fill `threshold_class`,
`threshold_trained`, `threshold_adjusted` and `threshold_n_moved`.

## Usage

``` r
build_corrections_export(corrections, custom_classes, thresholds = NULL)
```

## Arguments

- corrections:

  Corrections log data.frame (`rv$corrections`).

- custom_classes:

  Custom classes data.frame (`rv$custom_classes`).

- thresholds:

  Optional data.frame from
  [`threshold_summary()`](https://nodc-sweden.github.io/ifcb-algaware/reference/threshold_summary.md).

## Value

A data.frame ready for
[`utils::write.csv()`](https://rdrr.io/r/utils/write.table.html).
