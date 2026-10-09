# Rows whose stored label came from the classifier's threshold rule

Rows whose stored label came from the classifier's threshold rule

## Usage

``` r
follows_threshold_rule(classifications)
```

## Arguments

- classifications:

  A data.frame with `class_name` and `class_auto` columns.

## Value

Logical vector: `TRUE` where the stored label is the top-scoring class
or `"unclassified"`, so a threshold change may relabel the row. All
`FALSE` without a `class_auto` column.
