# Check one imported threshold row

Check one imported threshold row

## Usage

``` r
threshold_import_problem(cls, file_trained, adjusted, trained, tolerance)
```

## Arguments

- cls:

  Class name.

- file_trained:

  Trained threshold recorded in the file.

- adjusted:

  Adjusted threshold recorded in the file.

- trained:

  Named numeric vector of loaded trained thresholds, or `NULL`.

- tolerance:

  Allowed trained-threshold difference.

## Value

`NULL` when the row can be applied, otherwise the reason it cannot.
