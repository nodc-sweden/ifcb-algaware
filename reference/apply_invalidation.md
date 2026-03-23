# Apply class invalidation

Replaces invalidated class names with "unclassified" in the
classification data.

## Usage

``` r
apply_invalidation(classifications, invalidated_classes)
```

## Arguments

- classifications:

  A data.frame with a `class_name` column.

- invalidated_classes:

  Character vector of class names to invalidate.

## Value

A new data.frame with invalidated classes set to "unclassified".
