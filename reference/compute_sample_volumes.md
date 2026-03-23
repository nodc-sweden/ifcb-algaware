# Compute sample volumes per station visit

Compute sample volumes per station visit

## Usage

``` r
compute_sample_volumes(all_data)
```

## Arguments

- all_data:

  Data.frame with sample, visit_id, STATION_NAME, ml_analyzed,
  sample_time columns.

## Value

Data.frame with visit_id, STATION_NAME, total_ml_analyzed, median_time
columns.
