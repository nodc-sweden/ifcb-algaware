# Build the working classifications from the load-time snapshot

The single place where working labels are derived: the classifier output
as loaded, then threshold adjustments, then manual corrections (so a
manual decision always overrides a threshold), then the active-sample
filter.

## Usage

``` r
compose_classifications(
  original,
  adjustments,
  corrections,
  active_samples = NULL
)
```

## Arguments

- original:

  The load-time classifications (all samples).

- adjustments:

  Named numeric vector of adjusted thresholds, or `NULL`.

- corrections:

  Corrections log data.frame, or `NULL`.

- active_samples:

  Optional character vector of sample names to keep in the active slice.
  `NULL` keeps all samples.

## Value

A list with `all` (every sample) and `active` (only `active_samples`).
