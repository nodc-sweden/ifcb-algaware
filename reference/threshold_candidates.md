# Rows that can carry a class before or after a threshold change

An image can only be labelled with a class if it is the image's
top-scoring class, its stored label, or the target of a manual
correction. All other rows are irrelevant to a preview of that class.
The correction match is deliberately loose (sample and ROI number
matched separately, avoiding a key for every row); extra rows are
harmless.

## Usage

``` r
threshold_candidates(original, corrections, class_name)
```

## Arguments

- original:

  The load-time classifications.

- corrections:

  Corrections log data.frame, or `NULL`.

- class_name:

  Class of interest.

## Value

The subset of `original` that may be labelled `class_name`, in the
original row order.
