# Sample IDs of the active (non-excluded) samples

Sample IDs of the active (non-excluded) samples

## Usage

``` r
active_sample_ids(rv)
```

## Arguments

- rv:

  Reactive values with `matched_metadata_all` and `excluded_samples`.

## Value

Character vector of sample IDs, or `NULL` (all samples) when no metadata
is loaded.
