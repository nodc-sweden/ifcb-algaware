# Build a named vector of sample counts per station_date

Build a named vector of sample counts per station_date

## Usage

``` r
build_sample_counts(station_summary)
```

## Arguments

- station_summary:

  Aggregated station data with `STATION_NAME_SHORT`, `visit_date`, and
  `n_samples` columns.

## Value

Named integer vector mapping station_date strings to sample counts, or
NULL if `n_samples` is not available.
