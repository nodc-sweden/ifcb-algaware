# Apply adjusted class thresholds to classifications

Re-evaluates the threshold rule used by ifcb-classify for the adjusted
classes only: an image whose top-scoring class (`class_auto`) is an
adjusted class gets that class when its score is at least the adjusted
threshold, otherwise `"unclassified"`. Images of classes without an
adjustment keep their stored `class_name`, so an empty `adjustments`
leaves the data untouched.

## Usage

``` r
apply_thresholds(classifications, adjustments)
```

## Arguments

- classifications:

  A data.frame from
  [`read_h5_classifications()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_h5_classifications.md)
  with `class_name`, `class_auto` and `score` columns.

- adjustments:

  Named numeric vector (class name -\> adjusted threshold), or
  `NULL`/empty for none.

## Value

A data.frame like `classifications` with updated `class_name`.

## Details

Rows whose stored label is neither `class_auto` nor `"unclassified"` did
not come from the threshold rule and are never changed, nor are rows
without `class_auto`.
